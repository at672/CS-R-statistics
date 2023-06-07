setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list=ls())

CokePepsi = read.table("CokePepsi.csv", header=T)
price = CokePepsi[,1]
returns = diff(price)/lag(price)[-1] #These are most likely log returns
ts.plot(returns)
S = 4000
alpha = 0.05
library(MASS)
res = fitdistr(returns, 't')
mu = res$estimate['m']
lambda = res$estimate['s']
nu = res$estimate['df']
qt(alpha, df=nu)
dt(qt(alpha, df=nu), df=nu)

##PROBLEM 1:
#The line of code qt is computing the alpha quantile of a t distribution with our computed df
#The line of code dt(qt ...) is computing the value of the density function itself evaluated at that t-value

##PROBLEM 2:
Finv = mu + lambda * qt(alpha, df=nu) #computes the value at the alpha signifance level from the estimate t distribution
VaR = -S * Finv #computes VaR using 19.4 formula
options(digits=4)
den = dt(qt(alpha, df=nu), df=nu)
#apply formula 19.5
ES = S * (-mu + lambda*(den/alpha) * (nu+qt(alpha, df=nu)^2 )/(nu-1))


library(fGarch) # for qstd() function
library(rugarch)
garch.t = ugarchspec(mean.model=list(armaOrder=c(0,0)),
                        variance.model=list(garchOrder=c(1,1)),
                        distribution.model = "std")
KO.garch.t = ugarchfit(data=returns, spec=garch.t)
show(KO.garch.t)
plot(KO.garch.t, which = 2)
pred = ugarchforecast(KO.garch.t, data=returns, n.ahead=1) ; pred
fitted(pred) ; sigma(pred)
nu = as.numeric(coef(KO.garch.t)[5])
q = qstd(alpha, mean = fitted(pred), sd = sigma(pred), nu = nu) ; q
sigma(pred)/sqrt( (nu)/(nu-2) )
qt(alpha, df=nu)
dt(qt(alpha, df=nu), df=nu)

##PROBLEM 3: Can't be done in code. It is an ARMA GARCH model with the parameters as specified

##PROBLEM 4
# By reading the output of pred (the ugarch package forecast) we see the 
# conditional mean prediction is 0.0006348 (column series)
# and the conditional variance prediction is 0.01039 (sigma)

## PROBLEM 5
mu = as.numeric(fitted(pred))
lambda = as.numeric(sigma(pred)/sqrt( (nu)/(nu-2) ))
Finv = mu + lambda * qt(alpha, df=nu)
VaR = -S * Finv
options(digits=4)
VaR
den = dt(qt(alpha, df=nu), df=nu)
ES = S * (-mu + lambda*(den/alpha) * (nu+qt(alpha, df=nu)^2 )/(nu-1))
ES
#The value at risk prediction is now 63.4 and the expected shortfall is 89.46



#19.11.2 VaR Using a Multivariate-t Model
library(mnormt)
berndtInvest = read.csv("berndtInvest.csv")
Berndt = berndtInvest[,5:6]
names(Berndt)
Y = Berndt
loglik = function(par)
{
  mu = par[1:2]
  A = matrix(c(par[3], par[4], 0, par[5]), nrow = 2, byrow = T)
  scale_matrix = t(A) %*% A
  df = par[6]
  -sum(log(dmt(Y, mean = mu, S = scale_matrix, df = df)))
}
A = chol(cov(Y))
start = as.vector(c(apply(Y, 2, mean),
                    A[1, 1], A[1, 2], A[2, 2], 4))
fit_mvt = optim(start, loglik, method = "L-BFGS-B",
                lower = c(-0.02, -0.02, -0.1, -0.1, -0.1, 2),
                upper = c(0.02, 0.02, 0.1, 0.1, 0.1, 15), hessian = T)

## PROBLEM 6
# The estimates of the mean vector are par1:2, 0.004113 and 0.02 for DATGEN and DEC respectively
# The estimates of the degrees of freedom is value 6 of the fit
# For a univariate distribution, the df = the tail index.
# For a multivariate t distribution, it is the same
# The estimates of the cholesky matrix are parameters 3, 4, 5, for values 1,1, 1,2, and 2,2 respectively
# Recall Cov Matrix = nu/(nu-2) * Scale matrix, nu = degrees freedom
# if A is upper triangular matrix (comes from optim params), then transpose(A) * A = cov matrix
# from this we can back out the scale matrix.

# For univariate t: Therefore, the t-distribution has polynomial tails with tail index a = ν. The
# smaller the value of ν, the heavier the tails. page 96

# Regression estimator of tail index: −1/β_hat1
# hill estimator is 19.32 on page 568

## PROBLEM 7
weights = as.matrix( c(0.3, 0.7), nrow = 2, ncol = 1)
#my_vector <- c(fit_mvt$par[3], fit_mvt$par[4], 0, fit_mvt$par[5])
#A = matrix(data = my_vector, nrow = 2, ncol = 2, byrow = TRUE)
ML = fit_mvt$par

mean_vect = as.matrix( ML[1:2], nrow = 2)
Ahat = matrix(c(ML[3:4],0,ML[5]),nrow=2,byrow=TRUE)
COV_Y = t(Ahat)%*%Ahat * ML[6]/(ML[6]-2)

port_var = t(weights) %*% COV_Y %*% weights
port_mean = t(weights) %*% mean_vect

#The distribution of returns is t(mean = port_mean, var = port_var, df = df)
#For a $100,000 portfolio, simply multiply by 100,000

## 7b) use formulas 19.10 and 19.8 respectively for VaR and ES
#sd = scale * sqrt(nu/(nu-2))
port_sd = sqrt(port_var)
port_scale = port_sd/(sqrt(ML[6]/(ML[6]-2)))
alpha = 0.05
S = 1e5

mu = port_mean
lambda = port_scale
nu = ML[6]
Finv = mu + lambda * qt(alpha, df=nu) #computes the value at the alpha signifance level from the estimate t distribution
VaR = -S * Finv #computes VaR using 19.4 formula
options(digits=4)
den = dt(qt(alpha, df=nu), df=nu)
#apply formula 19.5
ES = S * (-mu + lambda*(den/alpha) * (nu+qt(alpha, df=nu)^2 )/(nu-1))