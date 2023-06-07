setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list=ls())

TbGdpPi = read.csv("TbGdpPi.csv", header=TRUE)
# r = the 91-day treasury bill rate
# y = the log of real GDP
# pi = the inflation rate
TbGdpPi = ts(TbGdpPi, start = 1955, freq = 4)
Tbill = TbGdpPi[,1]
Tbill.diff = diff(Tbill)
plot(Tbill)
plot(Tbill.diff)
acf(Tbill)
acf(Tbill.diff)
library(tseries)
adf.test(Tbill)
adf.test(Tbill.diff)
kpss.test(Tbill)
kpss.test(Tbill.diff)

#Problem 1:
# Visually, the plot of the Tbill is clearly nonstationary, but the plot for Tbill.diff looks stationary at first glance
# The ACF of Tbill confirms it is a nonstationary process, and Tbill.diff's acf suggests stationarity

#The ADF test for Tbill has a high p-value, so we fail to reject the null hypothesis, conclucing that Tbill is nonstationary
#The ADF test for TBILL.DIFF has a low p-value, significant at the 99% level, so we reject the null hypothesis and conclude that
#the series Tbill.diff is stationary

#The KPSS test for Tbill has a low p-value, so we reject the null hypothesis, which for a KPSS test is that the series is stationary.
# Thus we conclude Tbill is nonstationary
# The KPSS for Tbill.diff has a p-value of 0.1, which is on the high end, so we fail to reject the null, which is that Tbill.diff is stationary

#The hetereoskedasticity we can see in Tbill.diff is 

# 1) volatility clustering: present around 1978 to 1985, when the variance increases dramatically
# 2) general volatility clustering elsewhere (1960, 2010)

#Problem 2
library(rugarch)
arma.garch.norm = ugarchspec(mean.model=list(armaOrder=c(1,0)), variance.model=list(garchOrder=c(1,1)))
Tbill.diff.arma.garch.norm = ugarchfit(data=Tbill.diff, spec=arma.garch.norm)
show(Tbill.diff.arma.garch.norm)

#Problem 2 (a) : The GARCH model fit is (1,1), and the ARMA model fit is (1,0)
# b) the garch(p,q) parameters are p: alpha1 = 0.383, q: beta1 = 0.615, and omega = 0.0169
# the arma parameters are the mu = 0.185, and ar1 = 0.1396

## PROBLEM 3
res = ts(residuals(Tbill.diff.arma.garch.norm, standardize=FALSE),
            start = 1955, freq = 4)
res.std = ts(residuals(Tbill.diff.arma.garch.norm, standardize=TRUE),
                start = 1955, freq = 4)
par(mfrow=c(2,3))
plot(res)
acf(res)
acf(res^2)
plot(res.std)
acf(res.std)
acf(res.std^2)

# a) acf(res) Plots the ACF of the residuals. The plot tells us that the fit is not that great, since we see
# that the residuals are not a white noise process. But, if you do a Ljung-Box test on the standardized residuals,
# we get some evidence that the model is adequate

# (b) acf(res^2) Plots the acf of the square of the residuals. Because we see substantial autocorrelation of the
# squared residuals, we have reason to believe that there is conditional hetereoskedasticity present. This is not a concern
# because we are fitting a GARCH model.
# (c): This plots the square of the standardized residuals, and suggests that the the fit is good. The standardized residuals
# 
# (d): What's noticeable is the number of drawdowns, which suggest heavy tailed behavior, but to check for that we really need
# a QQ plot.


## PROBLEM 4
Tbill.arma.garch.norm = Tbill.diff.arma.garch.norm
diff.log.Tbill= diff(log(Tbill))
Tbill.arma.garch.norm.log = ugarchfit(data=diff.log.Tbill, spec=arma.garch.norm)
show(Tbill.arma.garch.norm.log)
res.log = ts(residuals(Tbill.arma.garch.norm.log, standardize=FALSE),
             start = 1955, freq = 4)
res.std.log = ts(residuals(Tbill.arma.garch.norm.log, standardize=TRUE),
                 start = 1955, freq = 4)
par(mfrow=c(2,3))
plot(res.log)
acf(res.log)
acf(res.log^2)
plot(res.std.log)
acf(res.std.log)
acf(res.std.log^2)


## 14.16.2 The GARCH-in-Mean (GARCH-M) Model

# Problem 5
library(rugarch)
GPRO = read.table("GPRO.csv")
garchm = ugarchspec(mean.model=list(armaOrder=c(0,0),
                                      archm=T,archpow=1),
                     variance.model=list(garchOrder=c(1,1)))
GPRO.garchm = ugarchfit(garchm, data=GPRO)
show(GPRO.garchm)

# The fitted model is a GARCH-M model with delta = archm = 2.09
# omega, alpha1, and beta1 are specified as below. The explicit form is in the textbook on page 445.

# Problem 6
# From the output we see the corresponding t-value and Pr(>{t}) for the parameters


## 14.16.3 Fitting Multivariate GARCH Models
# Problem 7

TbGdpPi = read.csv("TbGdpPi.csv", header=TRUE)
TbPi.diff = ts(apply(TbGdpPi[,-2],2,diff), start=c(1955,2), freq=4)
plot(TbPi.diff)
acf(TbPi.diff^2)
source("SDAFE2.R")
mLjungBox(TbPi.diff^2, lag=8)

# The MV Ljung-Box test shows a low p-value. We reject the null. The data exhibits a serial correlation
# When the square of the differences series exhibits a serial correlation, this indicates that the
# series does have MV conditional hetereoskedasticity

#Problem 8
EWMA.param = est.ewma(lambda.0=0.95, innov=TbPi.diff)
EWMA.param$lambda.hat
EWMA.Sigma=sigma.ewma(lambda=EWMA.param$lambda.hat,innov=TbPi.diff)
par(mfrow = c(2,2))
plot(ts(EWMA.Sigma[1,1,]^.5, start = c(1955, 2), frequency = 4),
        type = 'l', xlab = "year", ylab = NULL,
        main = expression(paste("(a) ", hat(sigma)["1,t"])))
plot(ts(EWMA.Sigma[1,2,], start = c(1955, 2), frequency = 4),
        type = 'l', xlab = "year", ylab = NULL,
        main = expression(paste("(b) ", hat(sigma)["12,t"])))
plot(ts(EWMA.Sigma[1,2,]/(sqrt(EWMA.Sigma[1,1,]* EWMA.Sigma[2,2,])),
           start = c(1955, 2), frequency = 4),
        type = 'l', xlab = "year", ylab = NULL,
        main = expression(paste("(c) ", hat(rho)["12,t"])))
plot(ts(EWMA.Sigma[2,2,]^.5, start = c(1955, 2), frequency = 4),
        type = 'l', xlab = "year", ylab = NULL,
        main = expression(paste("(d) ", hat(sigma)["2,t"])))

# Just call EWMA.param$lambda.hat to get the value of the parameter and $lambda.hat.se to get the SE of it
# 0.866

# Problem 9
n = dim(TbPi.diff)[1]
d = dim(TbPi.diff)[2]
stdResid.EWMA = matrix(0,n,d)
for(t in 1:n){
  stdResid.EWMA[t,] = TbPi.diff[t,] %*% matrix.sqrt.inv(EWMA.Sigma[,,t])
  }
mLjungBox(stdResid.EWMA^2, lag=8)
# From the results of the Ljung Box test, we conclude there is conditional hetereoskedasticity.
# The Ljung Box test is being applied to the squared standardized residuals of the EWMA model.
# But the Ljung Box test returns a very low p-value. So we conclude that the EWMA model is insufficent.
# WE need to capture the behavior with a GARCH model

# Problem 10
DOC.test(TbPi.diff^2, 8)

# The p-values are all very high, suggesting that we fail to reject the null
# The null hypothesis is that conditional correlations are 0 for all i,j.
# Thus, we can apply univariate modeling after estimating a transform.
