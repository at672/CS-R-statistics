setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

rm(list=ls())
library("Ecdat")
?CPSch3
data(CPSch3)
dimnames(CPSch3)[[2]]
male.earnings = CPSch3[CPSch3[ ,3] == "male", 2]
sqrt.male.earnings = sqrt(male.earnings)
log.male.earnings = log(male.earnings)
par(mfrow = c(2, 2))
qqnorm(male.earnings ,datax = TRUE, main = "untransformed")
qqline(male.earnings,datax=TRUE,col="red",lwd=2)
qqnorm(sqrt.male.earnings, datax = TRUE,
       main = "square-root transformed")
qqline(sqrt.male.earnings,datax=TRUE,col="red",lwd=2)
qqnorm(log.male.earnings, datax = TRUE, main = "log-transformed")
qqline(log.male.earnings,datax=TRUE,col="red",lwd=2)
par(mfrow = c(2, 2))
boxplot(male.earnings, main = "untransformed")
boxplot(sqrt.male.earnings, main = "square-root transformed")
boxplot(log.male.earnings, main = "log-transformed")
par(mfrow = c(2,2))
plot(density(male.earnings), main = "untransformed")
plot(density(sqrt.male.earnings), main = "square-root transformed")
plot(density(log.male.earnings), main = "log-transformed")


#Problem 1
# SQrt does the best job

#Problem 2
library("MASS")
par(mfrow = c(1, 1))
boxcox(male.earnings ~ 1)
#zoom in on the high-likelihood region
boxcox(male.earnings ~ 1, lambda = seq(0.3, 0.45, 1 / 100))
#find the MLE
bc = boxcox(male.earnings ~ 1, lambda = seq(0.3, 0.45, by = 1 / 100),
            interp = FALSE)
ind = (bc$y == max(bc$y))
ind2 = (bc$y > max(bc$y) - qchisq(0.95, df = 1) / 2)
bc$x[ind]
bc$x[ind2]
#Problem 2 (a): ind is a vector of indexes that say where the MLE occurs (where the y values of the transformed data
# equal the maximum)
# ind2 is the vector of indexes where the the values of the box cox transform are above the 95% significance level
# i.e. which values of lambda (x) are in a 95% conf interval

# 2 (b) The effect of interp is whether or not to use a spline interpolation.
# By default this is true with lambda of length < 100, we turned it off here
# Spline interpolations help smooth the plot

# 2 (c): The MLE is 0.36
# 2 (d): 95% CI is (0.32, 0.40)
# 2 (e):
ind3 = (bc$y > max(bc$y) - qchisq(0.99, df = 1) / 2)
bc$x[ind3]
# 2e: the 99% CI is 0.31, 0.41


library("fGarch")
fit = sstdFit(male.earnings, hessian = TRUE)
# Problem 3: From reading the output, the estimate of the DF parameter is 3.03e-6, and the estimate of xi is
# -1.85e-04
# The fisher information matrix is a hessian matrix of the negative log likelihood function

#Problem 4:
# https://cran.r-project.org/web/packages/skewt/skewt.pdf
grid = seq(0, 50, by = 1 / 10)
#library("skewt")
#plot(dskt(grid, df = fit$estimate[3], gamma = fit$estimate[4] ), type = 'l', main = "Skew T distr with MLEs")

plot( x = grid, y = dsstd( x = grid, mean = fit$estimate[1],
                           sd = fit$estimate[2],
                           nu = fit$estimate[3],
                           xi = fit$estimate[4]),
      type = 'l',
      main = "Male Earnings",
      xlab = "Earnings in Thousands",
      ylab = "Frequency")
#Use lines or points function to add an overlay
lines(density(male.earnings), col = 'blue')

legend(x='bottomright', legend=c("Skew T MLE", "KDE"),
       col=c("black", "blue"), lty=1, cex=0.8, bg = 'lightblue', inset = 0.05)

#Problem 4 WRite up: The MLE fit Skew T and the KDE are both similar. The KDE tends to overfit, the skew t tends to underfit
# The skew tmodel does provide an adequate fit
# The reason for overfit/underfit is because the nonparametric is data based, and the parametric is data agnostic
# Though this can be adjusted with bandwidth for the nonparametric.


# Problem 5
#GED is generalized error distribution
grid = seq(0, 50, by = 1 / 10)
plot( x = grid, y = dsged( x = grid, mean = fit$estimate[1],
                           sd = fit$estimate[2],
                           nu = fit$estimate[3],
                           xi = fit$estimate[4]),
      type = 'l',
      main = "Male Earnings",
      xlab = "Earnings in Thousands",
      ylab = "Frequency")
#Use lines or points function to add an overlay
lines(density(male.earnings), col = 'blue')

legend(x='bottomright', legend=c("Skew GED MLE", "KDE"),
       col=c("black", "blue"), lty=1, cex=0.8, bg = 'lightblue', inset = 0.05)

#Problem 5 write up: The skewed GED model is a terrible fit compared to the skewed t model


#Problem 6: DAX Returns
data(Garch, package = "Ecdat")
library("fGarch")
data(EuStockMarkets)
Y = diff(log(EuStockMarkets[ ,1])) # DAX log returns
##### std #####
loglik_std = function(x) { #compute the negative log likelihood function
  f = -sum(dstd(Y, x[1], x[2], x[3], log = TRUE))
  f}
start = c(mean(Y), sd(Y), 4) #optimize the negative log likelihood with minimization
#we are trying to get MLEs for the mean, std dev, and deg freedom of the DAX Log returns
fit_std = optim(start, loglik_std, method = "L-BFGS-B",
                lower = c(-0.1, 0.001, 2.1),
                upper = c(0.1, 1, 20), hessian = TRUE)
cat("MLE =", round(fit_std$par, digits = 5))
minus_logL_std = fit_std$value # minus the log-likelihood
AIC_std = 2 * minus_logL_std + 2 * length(fit_std$par)

#Problem 6: The MLEs are fit_std$par, [1], [2], and [3] for mean, sd, and df respectively
# I.E. they are 0.00078, 0.1056, and 4.035
# The value of AIC is AIC_std which is -11960.47


#Problem 7
loglik_sstd = function(x) { #compute the negative log likelihood function
  f = -sum(dsstd(Y, x[1], x[2], x[3], x[4], log = TRUE))
  f}
start2 = c(mean(Y), sd(Y), 4, 1) #optimize the negative log likelihood with minimization
fit_sstd = optim(start2, loglik_sstd, method = "L-BFGS-B",
                lower = c(-0.1, 0.001, 2.1, 0.25),
                upper = c(0.1, 1, 20, 4), hessian = TRUE,
                control = list(maxit=1000, tmax = 100))
cat("MLE =", round(fit_sstd$par, digits = 5))
minus_logL_sstd = fit_sstd$value # minus the log-likelihood
AIC_sstd = 2 * minus_logL_sstd + 2 * length(fit_sstd$par)
#confidence interval construction
se = sqrt(diag(solve(fit_sstd$hessian)))
ci = matrix(nrow=4,ncol=2)
for (i in 1:4){
  ci[i,] = fit_sstd$par[i] + c(-1,1)*se[i]*qnorm(0.975)
}

#The MLEs are computed as above. We can see that the neg log likelihood is a higher value
# The skew parameter is almost 1, and the df parameter changed a bit
# The AIC is -11958.75.
# Since AIC selects the model with the lowest AIC value, we determine that AIC chooses model 1, i.e. non-skewed

#starting vector matters! If you start with xi = 1.5, it reaches a different local minimum with different AIC

#Problem 8: page 130
#compute the transformation

x1 = pstd(Y, mean = fit_std$par[1], sd = fit_std$par[2], nu = fit_std$par[3])
x = qnorm(x1)
# x is the transformed data
plot(density(x), type = 'l', main = "Transformed DAX Log Returns")


#Problem 9
#Compute the KDE of the Untransformed data
d1 = density(Y)
#Compute the KDE of the transformed data, which is NOT the TKDE
d2 = density(x)

#Compute the derivative which is used for ...
ginvx = qstd(pnorm(d2$x), mean = fit_std$par[1], sd = fit_std$par[2], nu = fit_std$par[3])
gprime_num = dstd(ginvx, mean = fit_std$par[1], sd = fit_std$par[2], nu = fit_std$par[3])
gprime_den = dnorm(qnorm(pstd(ginvx, mean = fit_std$par[1], sd = fit_std$par[2], nu = fit_std$par[3])))
gprime = gprime_num / gprime_den

plot(d1$x, d1$y, type = "l", xlab = "y", ylab = "Density(y)", lwd = 2) #Plot the KDE
lines(ginvx, d2$y * gprime, type = "l", lty = 2, col = "red", lwd = 2) #Plot the TKDE

#Plot the parametric estimator
gr = seq(-0.05, 0.05, by = 1 / 500)
lines( x= gr, y = dstd(x = gr, mean = fit_std$par[1], sd = fit_std$par[2], nu = fit_std$par[3]), type = 'l', col = 'blue')
legend("topleft", c("KDE", "TKDE", "Parametric"), lty = c(1,2), lwd = 2, col = c("black", "red", "blue"))

#Zoomed in 


plot(d1$x, d1$y, type = "l", xlab = "y", ylab = "Density(y)", lwd = 2, xlim = c(0.035, 0.06), ylim = c(0, 0.8)) #Plot the KDE
lines(ginvx, d2$y * gprime, type = "l", lty = 2, col = "red", lwd = 2) #Plot the TKDE
#Plot the parametric estimator
gr = seq(-0.05, 0.05, by = 1 / 500)
lines( x= gr, y = dstd(x = gr, mean = fit_std$par[1], sd = fit_std$par[2], nu = fit_std$par[3]), type = 'l', col = 'blue')
legend("topleft", c("KDE", "TKDE", "Parametric"), lty = c(1,2), lwd = 2, col = c("black", "red", "blue"))

#Problem 9 write up: When zooming in, we see that the KDE is untsable on the tails. The TKDE fixes this problem quite nicely
#The TKDE and parameetric models seem to agree with one another on the tails


#Problem 10
#Skipped because it's basically a repeat of an earlier problem

#Problem 11
data = read.csv('MCD_PriceDaily.csv')
adjPrice = data[ ,7]
LogRet = diff(log(adjPrice))
library(MASS)
library(fGarch)
fit.T = fitdistr(LogRet, "t")
params.T = fit.T$estimate
mean.T = params.T[1]
sd.T = params.T[2] * sqrt(params.T[3] / (params.T[3] - 2))
nu.T = params.T[3]
x = seq(-0.04, 0.04, by = 0.0001)
hist(LogRet, 80, freq = FALSE)
lines(x, dstd(x, mean = mean.T, sd = sd.T, nu = nu.T), lwd = 2, lty = 2, col = 'red')

#Problem 11: The first line loads in data. The next two computes log returns. The two after that load in libraries
#The next one fits the log returns to a t distribution. The next four lines extract the values of the mean, sd, and df
# with the line for sd converting from the scale factor (which is the value in the estimate) to the sd
# x is created as a linearly spaced vector, and then the histogram is plotted with a dashed line plot of the t-distr fit

#Problem 12
test_res = t.test( LogRet )
#We fail to reject the Null hypothesis at the 5% confidence level, but we do have evidence to reject the null
#hypothesis at the 10% confidence level. The p-value is 0.07469

#Problem 13
# I think the parametric fit is adequate. It seems to fit quite nicely

#Problem 14
#How heavy is the tail of the parametric fit? Does it appear that
#the fitted t-distribution has a finite kurtosis? How confident are you that the
#kurtosis is finite?

#Answer: Kurtosis of a t-distr is 3 + 6/(nu-4) if nu > 4, and then it is finite. If nu <= 4, it is infinite
#Since the df is estimated at 4.269, the kurotsis is finite and it is
kurt = 3 + 6/(nu.T - 4)
#Since the df is small, we conclude the tail is pretty heavy. The t distribution has polynomial tails which are heavy to begin with.

#





