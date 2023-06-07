setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list=ls())

library(copula)
cop_t_dim3 = tCopula(dim = 3, param = c(-0.6,0.75,0),
                       dispstr = "un", df = 1)
set.seed(5640)
rand_t_cop = rCopula(n = 500, copula = cop_t_dim3)
pairs(rand_t_cop)
cor(rand_t_cop)

#Problem 1
# (a) : A 3 dimensional t copula has been sampled. The degrees of freedom is 1.
#       dispstr = "un" signifies that the correlation matrix is unstructured, i.e. is arbitrary.
#     The correlation matrix is [ 1, -0.6, 0.75;
#                                 -0.6  1     0;
#                                 0.75  0     1]

# b sample size is 500

#Problem 2
# #a: If they were independent, they would be uniformly scattered. They are not uniformly scattered
# because there is some heaviness at the tail ends. We can see a faint outline of an "X" shape

# b: Tail dependence is when an extreme return for one security lines up with an extreme return for the second security
# From part a we know that there is more data in the corners, i.e. more extreme events for both securities.
# Thus, we conclude that there is tail dependence between  var 2 and var 3, var 1 and var 2, and var 1 and var 3
# Note: Tail dependence present with two components that are uncorrelated mean that when one takes an extreme positive value
# the other is equally likely to be an extreme positive value or an extreme negative value.

# c: We see tail dependence most clearly on var1, var 2 plots and var 1 var 3 plots. On the v1v2 plot, we see in the
# upper left corner and the bottom right corner concentration because they are negatively correalted
# for the v1v3 plots, since they are positive correlation, we see concentration in the bottom left, and top right



cor.test(rand_t_cop[,1],rand_t_cop[,3])

# d: The 95% CI does not capture 0.75. Usually sample correlations not mtaching the true correlation of the 
# distribution sampled from is due to random varitaion.

# However, 0.75 is the correlation between the t-distributed random variables var 2 and var 3
# That is not the same as the uniformly-distributed variables in the copula itself.


# Problem 3 code
cop_normal_dim3 = normalCopula(dim = 3, param = c(-0.6,0.75,0),
                               dispstr = "un")
mvdc_normal = mvdc(copula = cop_normal_dim3, margins = rep("exp",3),
                      paramMargins = list(list(rate=2), list(rate=3),
                                             list(rate=4)))
set.seed(5640)
rand_mvdc = rMvdc(n = 1000, mvdc = mvdc_normal)
pairs(rand_mvdc)
par(mfrow = c(2,2))
for(i in 1:3) plot(density(rand_mvdc[,i]))

#Prob 3: a) The marginals are all exponential distributions. The first one has a rate of 2 (mean of 0.5),
# the second has a rate of 3 (mean of 0.3333), and the third has a rate of 4 (mean of 0.25)
# b) We know from how the matrix is constructed and supplie in the normalCopula call, that var 2 and var 3 should be independent
# We are confident that they are because we don't see a clear trend. We see clustering due to tail dependence and the behavior
# of the exponential distribution


#Problem 4
library(MASS) # for fitdistr() and kde2d() functions
library(copula) # for copula functions
library(fGarch) # for standardized t density
netRtns = read.csv("IBM_SP500_04_14_daily_netRtns.csv", header = T)
ibm = netRtns[,2]
sp500 = netRtns[,3]
est.ibm = as.numeric( fitdistr(ibm,"t")$estimate )
est.sp500 = as.numeric( fitdistr(sp500,"t")$estimate )
est.ibm[2] = est.ibm[2] * sqrt( est.ibm[3] / (est.ibm[3]-2) )
est.sp500[2] = est.sp500[2] * sqrt(est.sp500[3] / (est.sp500[3]-2) )
cor_tau = cor(ibm, sp500, method = "kendall")
#Omega is the sample pearson correlation.
omega = sin((pi/2)*cor_tau)
# P4: Omega was computed as 0.701
cop_t_dim2 = tCopula(omega, dim = 2, dispstr = "un", df = 4)

data1 = cbind(pstd(ibm, est.ibm[1], est.ibm[2], est.ibm[3]),
                 pstd(sp500, est.sp500[1], est.sp500[2], est.sp500[3]))
n = nrow(netRtns) ; n
data2 = cbind(rank(ibm)/(n+1), rank(sp500)/(n+1))
ft1 = fitCopula(cop_t_dim2, data1, method="ml", start=c(omega,4) )
ft2 = fitCopula(cop_t_dim2, data2, method="ml", start=c(omega,4) )

#Problem 5:
# a) Both ft1 and ft2 are fit using maximum likelihood, or pseudo-likelihood to be precise
# data1 is parametric since the marginals are estimated by fitting t distributions.
# data2 is nonparametric because it uses the empirical CDF

# b) Calling ft1 and ft2 in the console, we see that the correlations are degrees of freedom are very similar
# There is no significant practical difference.

# Problem 6
#define a meta-t-distribution
mvdc_t_t = mvdc( cop_t_dim2, c("std","std"), list(
  list(mean=est.ibm[1],sd=est.ibm[2], nu=est.ibm[3]),
  list(mean=est.sp500[1],sd=est.sp500[2], nu=est.sp500[3])))

#fit the meta t-distribution
start = c(est.ibm, est.sp500, ft1@estimate)
objFn = function(param) -loglikMvdc(param,cbind(ibm,sp500),mvdc_t_t)
tic = proc.time()
ft = optim(start, objFn, method="L-BFGS-B",
              lower = c(-.1,0.001,2.2, -0.1,0.001,2.2, 0.2,2.5),
              upper = c( .1, 10, 15, 0.1, 10, 15, 0.9, 15) )
toc = proc.time()
total_time = toc - tic ; total_time[3]/60

#Problem 6 a) The estimates of the copula parameters are ft$par. The last two are the correlation and degrees of freedom estimate
# b) The estimates of the univariate parameters are the first 6. The first 3 for the first, the second three for the second
# b) The univariate parameters are mean, sd, and df
# c) The estimation was parametric pseudo-maximum likelihood because we fit the marginals to univariate t distributions
# (d) Estimate the coefficient of lower tail dependence for this copula.
# d) The coefficient for lower tail dependence is lim q↓0 C_Y (q, q) / q
# The derivation is left for more advanced books, as discussed in the chapter
# The result is
# lambda_l = 2F_t, nu+1{ -sqrt((nu+1)(1-rho)/(1+rho))  }
nu = ft$par[8]
rho = ft$par[7]
quantity = -sqrt( (nu+1)*(1-rho)/(1+rho)  )
lambda_l = 2 * pt(quantity, nu+1)
#Thus the coefficient of lower tail dependence is 0.4535

#Problem 7 code
fnorm = fitCopula(copula=normalCopula(dim=2),data=data1,method="ml")

ffrank = fitCopula(copula = frankCopula(3, dim = 2),
                      data = data1, method = "ml")
fclayton = fitCopula(copula = claytonCopula(1, dim=2),
                        data = data1, method = "ml")
fgumbel = fitCopula(copula = gumbelCopula(3, dim=2),
                       data = data1, method = "ml")
fjoe = fitCopula(copula=joeCopula(2,dim=2),data=data1,method="ml")

Udex = (1:n)/(n+1)
#Cn = C.n(u = cbind(rep(Udex,n), rep(Udex,each=n)), U=data1, method="C")
#50 MB, takes about a minute to run.
Cn = C.n( u = cbind(rep(Udex,n), rep(Udex,each=n)), X = data1)

Cn2 = empCopula(X = data1)

#In Package 1.1, the new way they prefer you to run.
EmpCop = expression(contour(Udex, Udex, matrix(Cn, n, n),
                               col = 2, add = TRUE))

par(mfrow=c(2,3), mgp = c(2.5,1,0))
contour(tCopula(param=ft$par[7],dim=2,df=round(ft$par[8])),
           pCopula, main = expression(hat(C)[t]),
           xlab = expression(hat(U)[1]), ylab = expression(hat(U)[2]) )
eval(EmpCop)
contour(normalCopula(param=fnorm@estimate[1], dim = 2),
           pCopula, main = expression(hat(C)[Gauss]),
           xlab = expression(hat(U)[1]), ylab = expression(hat(U)[2]) )
eval(EmpCop)
contour(frankCopula(param=ffrank@estimate[1], dim = 2),
           pCopula, main = expression(hat(C)[Fr]),
           xlab = expression(hat(U)[1]), ylab = expression(hat(U)[2]) )
eval(EmpCop)
contour(claytonCopula(param=fclayton@estimate[1], dim = 2),
           pCopula, main = expression(hat(C)[Cl]),
           xlab = expression(hat(U)[1]), ylab = expression(hat(U)[2]) )
eval(EmpCop)
contour(gumbelCopula(param=fgumbel@estimate[1], dim = 2),
           pCopula, main = expression(hat(C)[Gu]),
           xlab = expression(hat(U)[1]), ylab = expression(hat(U)[2]) )
eval(EmpCop)
contour(joeCopula(param=fjoe@estimate[1], dim = 2),
           pCopula, main = expression(hat(C)[Joe]),
           xlab = expression(hat(U)[1]), ylab = expression(hat(U)[2]) )
eval(EmpCop)

#Problem 7: C_hat t is our parametric estimated t copula.
#From observing the red lines(the empirical copula) and the black lines(the fits we chose)
# We know the best fit is where the lines are closeest to one onther
# unsurprisingly, this is for the parametric estimate of the t copula

#Problem 8

par(mfrow=c(2,3), mgp = c(2.5,1,0))
contour(tCopula(param=ft$par[7],dim=2,df=round(ft$par[8])),
           dCopula, main = expression(hat(c)[t]),
           nlevels=25, xlab=expression(hat(U)[1]),ylab=expression(hat(U)[2]))
contour(kde2d(data1[,1],data1[,2]), col = 2, add = TRUE)
contour(normalCopula(param=fnorm@estimate[1], dim = 2),
           dCopula, main = expression(hat(c)[Gauss]),
           nlevels=25, xlab=expression(hat(U)[1]),ylab=expression(hat(U)[2]))
contour(kde2d(data1[,1],data1[,2]), col = 2, add = TRUE)
contour(frankCopula(param=ffrank@estimate[1], dim = 2),
           dCopula, main = expression(hat(c)[Fr]),
           nlevels=25, xlab=expression(hat(U)[1]),ylab=expression(hat(U)[2]))
contour(kde2d(data1[,1],data1[,2]), col = 2, add = TRUE)
contour(claytonCopula(param=fclayton@estimate[1], dim = 2),
           dCopula, main = expression(hat(c)[Cl]),
           nlevels=25, xlab=expression(hat(U)[1]),ylab=expression(hat(U)[2]))
contour(kde2d(data1[,1],data1[,2]), col = 2, add = TRUE)
contour(gumbelCopula(param=fgumbel@estimate[1], dim = 2),
           dCopula, main = expression(hat(c)[Gu]),
           nlevels=25, xlab=expression(hat(U)[1]),ylab=expression(hat(U)[2]))
contour(kde2d(data1[,1],data1[,2]), col = 2, add = TRUE)
contour(joeCopula(param=fjoe@estimate[1], dim = 2),
           dCopula, main = expression(hat(c)[Joe]),
           nlevels=25, xlab=expression(hat(U)[1]),ylab=expression(hat(U)[2]))
contour(kde2d(data1[,1],data1[,2]), col = 2, add = TRUE)

#Problem 8: Interestingly, the gaussian copula is closest to the KDE

#Problem 9:
#same omega that estimates pearson correlation from the start of this lab.
AIC = c(0,0,0,0,0,0)
AIC[1] = -2*ft$value + 2*length(ft$par)
# Gauss Copula
AIC[2] = -2*fnorm@loglik + 2*length(fnorm@estimate)
# Frank copula
AIC[3] = -2*ffrank@loglik + 2*length(ffrank@estimate)
# Clayton Copula
AIC[4] = -2*fclayton@loglik + 2*length(fclayton@estimate)
# Gumbel Copula
AIC[5] = -2*fgumbel@loglik + 2*length(fgumbel@estimate)
# Joe Copula
AIC[6] = -2*fjoe@loglik + 2*length(fjoe@estimate)

#As we see, 

#> AIC
#[1] -13640.438  -1666.648  -1562.383  -1460.880  -1735.932  -1379.822
# The lowest AIC is for the parametric model, i.e. the meta t. This concurs with our earlier conclusions

