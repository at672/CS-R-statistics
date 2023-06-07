setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list=ls())
library("Ecdat")
library("forecast")
data(IncomeUK)
consumption = IncomeUK[,2]
plot(consumption)

# Problem 1
# Consumption appears to be a non-stationary process. We can see the mean increasing over time
# I recommend non-seasonal differencing to make the process stationary
# And I also recommend seasonal differencing because we can see peaks-and valley
# Behavior in the two months immediately after the new year

# I think fitting a seasonal ARIMA model to the data with a log transform would work well.
# The plot shows that as time progresses, the variation increases, giving credence to us using a log transform

plot(log(consumption))

#The log plot shows a nonstationary process, but it doesn't show the variation increasing as much

acf(consumption)

#The acf shows strong autocorrelation, confirming its nonstationarity

#Problem 2
model = auto.arima(y = log(consumption), seasonal = TRUE )

# From the output, the model chosen was differenced once and seasonally differenced once, and then was fit with 
# a seasonal MA(1)

# Problem3 
acf(model$residuals)
# The ACF plot of the residuals shows a what appears to be a white noise process, which
# means that the model selected is pretty good

# Problem 4
model_bic = auto.arima(y = log(consumption), seasonal = TRUE, ic = "bic" )
#BIC chose the same model

#Problem 5. Since the model is the same, I'm only doing it once here:
logConsumption = log(consumption)
fitAutoArima = auto.arima(logConsumption, ic="bic")
foreAutoArima = forecast(fitAutoArima, h=8)
plot(foreAutoArima, xlim=c(1985.5,1987.5), ylim=c(10.7,11.2))


# 13.8.2 Regression with HAC Standard Errors

data(Mishkin, package="Ecdat")
tb1_dif = diff(as.vector(Mishkin[,3]))
tb3_dif = diff(as.vector(Mishkin[,4]))
fit = lm(tb1_dif ~ tb3_dif )
round(summary(fit)$coef, 4) #These are the OLS coefficients
acf(fit$resid)
# Problem 6
# Yes, there is evidence of significant autocorrelation among the residuals
# We can see some pretty high values of the ACF at lags 1, arn around 12, 13

# Problem 7:

library(sandwich)
sqrt(diag(NeweyWest(fit, lag = 0, prewhite = F)))
coef(fit)/sqrt(diag(NeweyWest(fit, lag = 0, prewhite = F)))

#see page 376 in the book. The regression estimates are either from OLS, or sqrt(diag(NeweyWest))
#The corresponding t values are the OLS coefficient estimates divided by their
#standard error estimates, i.e. line 64 here.

#Answer to Problem 7: The t-values from the OLS fit for the intercept is 0.0001 and 25.5559 for the slope of tb3_diff
#It is 1.16e-04 for the intercept and 11.784 for the tb3_diff slope.
#The t-value is higher for the intercept and lesser for the slope coming from the HAC construction.
#The t-value for tb3_diff is still statistically singificant, and for intercept is still statistically insignificant

#Problem 8

sqrt(diag(NeweyWest(fit, lag = 1, prewhite = F)))
coef(fit)/sqrt(diag(NeweyWest(fit, lag = 1, prewhite = F)))

sqrt(diag(NeweyWest(fit, lag = 2, prewhite = F)))
coef(fit)/sqrt(diag(NeweyWest(fit, lag = 2, prewhite = F)))

sqrt(diag(NeweyWest(fit, lag = 3, prewhite = F)))
coef(fit)/sqrt(diag(NeweyWest(fit, lag = 3, prewhite = F)))

#What we observe is that by increasing the lag, the estimates are decreasing and as a result the t-values are increasing

#13.8.3 Regression with ARMA Noise
#Problem 9

library(AER)
data("USMacroG")
MacroDiff = as.data.frame(apply(USMacroG, 2, diff))
attach(MacroDiff)
fit1 = arima(unemp, order=c(1,0,0), xreg=cbind(invest, government))
fitAR1 = fit1
fitAR0 = arima(unemp, order=c(0,0,0), xreg=cbind(invest, government))
fitLM = lm(unemp ~ invest+government)
options(digits = 4)
fitLM$coef
fit1$coef
par(mfrow=c(1,2))
acf(residuals(fitLM))
acf(residuals(fit1))
fitAR0$aic
fitAR1$aic

#WE fit an AR0 model because the lm function does not provide us with AIC
#lm = AR0 assumption
# By comparing AIC, we see that the fit with an AR1 has a lower AIC, indicating it is a better model
#The ACF plots of the residuals also confirms that the AR1 assumption provides a better fit

#Problem 10
n = length(unemp)
fitAR0$bic = fitAR0$aic + (log(n)-2)*2
fitAR1$bic = fitAR1$aic + (log(n)-2)*2

fitAR0$bic
fitAR1$bic

#We see with using BIC as the criterion, the fit AR1 is still better


#Problem 11
fitAR2 = arima(unemp, order=c(2,0,0), xreg=cbind(invest, government))
fitARMA1_1 = arima(unemp, order=c(1,0,1), xreg=cbind(invest, government))
par(mfrow=c(1,2))
acf(residuals(fitAR2))
acf(residuals(fitARMA1_1))
fitAR2$aic
fitARMA1_1$aic
fitAR2$bic = fitAR2$aic + (log(n)-2)*2
fitARMA1_1$bic = fitARMA1_1$aic + (log(n)-2)*2
fitAR2$bic
fitARMA1_1$bic

#From both AIC and BIC, AR2 is superior to ARMA1,1 and to AR1.
#From the plots, it's difficult to tell the difference
# This is possibly related to the fact that the difference in AIC/BIC is small

#13.8.4 VAR Models
#Problem 12
TbGdpPi = read.csv("TbGdpPi.csv", header=TRUE)
# r = the 91-day treasury bill rate
# y = the log of real GDP
# pi = the inflation rate
TbGdpPi = ts(TbGdpPi, start = 1955, freq = 4) #convert data to time series with quarterly frequency (sampling?)
del_dat = diff(TbGdpPi)
var1 = ar(del_dat, order.max=4, aic=T) #Fit AR model
var1
acf(na.omit(var1$resid))

#Problem 12:
# From calling var1$order, we see this is an AR(3) model
# From calling var1$ar, we do NOT get the matricies Phi1, Phi2, etc. We get what appear to be the coefficients
# But if you just type "var1" and look at the "$ar" output, you see the matricies phi, ordered 1, 2, and 3

# (b): The estimated covariance matrix of eps_t is from the output, var.pred
# (c): At a cursory glance, we do see a few residual correlations out of bounds, noticeably on y&r at 0 and 2, and r&pi at 2.5
# To check for certain, perform a multivariate Ljung Box test.
# From the professor's solutions, the MV Ljung Box test did show statistical significance (of cross correlations)
# However, lesson here is that statistical significance =/= practical significance
# Visually all the cross correlations look low, so chances are the model can work for predictions or other applications

# Problem 13
last_four_values = tail(TbGdpPi, n = 4)
#The question wants us to Predict Y_n+1, I.E. the next value
#Use the function specified in book on page 387, eqn 13.20
mu = matrix(data = var1[4]$x.mean,
            dimnames = list(c("r","y","pi"), c("1")))

#Phi1. Use data = var1$ar[1,,] next time
Phi1 = matrix(data = c(-0.145511, 16.206, -0.098192,
              0.000517,  0.292,  0.000949,
              0.182083,  5.028, -0.805748),
              dimnames = list(c("r","y","pi"), c("r","y","pi")), nrow = 3, ncol = 3, byrow = TRUE  )
#Phi2 use data = var1$ar[2,,] next time
Phi2 = matrix(data = c(-0.22720, 12.795,  0.032919,
                       -0.00198,  0.178,  0.000382,
                       0.00166, -3.039, -0.685300
                       ),
              dimnames = list(c("r","y","pi"), c("r","y","pi")), nrow = 3, ncol = 3, byrow = TRUE  )


Phi3 = matrix(data = c(0.227678, -2.7642,  0.030225,
                       -0.000772, -0.0163,  0.000416,
                       0.154094, 13.2299, -0.513556
                       ),
              dimnames = list(c("r","y","pi"), c("r","y","pi")), nrow = 3, ncol = 3, byrow = TRUE  )
#The tail command which we use to write to last_four_values gives us values in reverse order, so we must reverse it again

Y_N_minus_0 = matrix(last_four_values[4,])
Y_N_minus_1 = matrix(last_four_values[3,])
Y_N_minus_2 = matrix(last_four_values[2,])
Y_N_minus_3 = matrix(last_four_values[1,])

Prediction = mu + Phi1%*%(Y_N_minus_0-mu) + Phi2%*%(Y_N_minus_1-mu) + Phi3%*%(Y_N_minus_2-mu)
Prediction
#The Prediction is computed as a result 


## Problem 14
nrows = dim(TbGdpPi)[1] #ncols = dim(TbGdpPi)[2]
var1 = ar(del_dat, order.max=1)
yn = var1$x.mean * 1.1 ; yn
#This is Phi1: var1$ar[1,,]
mu = matrix(data = var1[4]$x.mean, dimnames = list(c("r","y","pi"), c("1")))
h = 5
# Create a "list" of length h that stores a matrix object of dimensions 3x1, and don't write any data to the matrix yet
#Forecast = replicate(h, matrix("", 3, 1), simplify = F)
#Forecast = array(NA, c(h, 3, 1))
# Data Structure not working the way I want
F1 = mu + var1$ar[1,,] %*%  (matrix(yn)-mu) #we treat yn as the most recent forecast as per the problem specification
F2 = mu + var1$ar[1,,] %*%  (F1-mu)
F3 = mu + var1$ar[1,,] %*%  (F2-mu)
F4 = mu + var1$ar[1,,] %*%  (F3-mu)
F5 = mu + var1$ar[1,,] %*%  (F4-mu)
R1 = F1/mu
R2 = F2/mu
R3 = F3/mu
R4 = F4/mu
R5 = F5/mu
#Problem 14 Explanation: The forecasts F1...F5 all are close to the mean, as expected. The ratios vary a bit, but
# by the time we get to F5 we see the ratios converge to 1., not 1.1. This suggests that the forecasts
# converge to the mean vector of the estimate, not the starting value of the forecast.
# If you rerun the code with yn = 1.3, R5 ends up being 1.013, 1.001, and 1.178, not 1.3
# This confirms our notion that it should converge to 1.0, not the ratio used for yn.

## PROBLEM 15
Phi_hat = var1$ar[1,,] ; Phi_hat
# I can't tell by looking at it. It suggests that the the r changes negatively wrt itself and wrt pi, but highly positively
# wrt y. Not sure what to answer  here.

## PROBLEM 16
eigen.values = eigen(Phi_hat)$values
abs(eigen.values)

#The estimated values are well below 1, meaning that the estimated process is indeed stationary.
# This concurs with the results from problem 14, suggesting that the forecasts should converge to the mean,
# since a stationary process implies constant mean.

## PROBLEM 17
MacroVars = read.csv("MacroVars.csv", head=TRUE)
varp = ar(MacroVars, order.max = 10, aic=T) #Fit AR model
acf(na.omit(varp$resid))
#The model selects an AR(10) model with AIC of 0. AR(5) is a close second with AIC of 0.5504

## PROBLEM 18
p = c(0,1,2,3,4,5,6,7,8,9,10)
n = dim(MacroVars)[1] #number of rows/observations, i.e. sample size
BIC = varp$aic + (log(n) - 2)*p
BIC

#When Examining BIC, We see that the AR(5) has a lower BIC to AR(10), so we choose AR(5) since it's simpler

# 13.8.5 Long-Memory Processes

#PROBLEM 19
data(Mishkin, package="Ecdat")
cpi = as.vector(Mishkin[,5])
DiffSqrtCpi = diff(sqrt(cpi))
plot(DiffSqrtCpi)
plot(as.ts(DiffSqrtCpi))
acf(DiffSqrtCpi)

#We see evidence of Long Memory Behavior most clearly from the ACF plot, which does not show a rapid decay to 0

##PROBLEM 20
library("fracdiff")
fit.frac = fracdiff(DiffSqrtCpi,nar=0,nma=0)
fit.frac$d
fdiff = diffseries(DiffSqrtCpi,fit.frac$d)
acf(fdiff)
# We do not see any substantial long or short term autocorrelation in the series. We do see some positive ACFs at the 9 and 12 values


## PROBLEM 21
library("forecast")
model_aic = auto.arima(fdiff, ic = "aic")
model_bic = auto.arima(fdiff, ic = "bic")

#We see that AIC chooses an ARIMA 5,1,4 and BIC chooses a 5,1,1. We expect that BIC will choose a simpler model
#The values of the AIC and BIC are all around -3000. We see a loss of about 34 when going from the BIC to the AIC model
#We choose the BIC model due to its simplicity


# 13.8.6 Model-Based Bootstrapping of an ARIMA Process

library(AER)
library(forecast)
data("FrozenJuice")
price = FrozenJuice[,1]
plot(price)
oj_model = auto.arima(price, ic="bic")

n = length(price)
sink("priceBootstrap.txt")
set.seed(1998852)
for (iter in 1:10){
  eps = rnorm(n+20)
  y = rep(0,n+20)
  for (t in 3:(n+20)){
    y[t] = 0.2825*y[t-1] + 0.0570*y[t-2] + eps[t]
     }
  y = y[101:n+20]
  y = cumsum(y)
  y = ts(y, frequency=12)
  fit = auto.arima(y, d=1, D=0, ic="bic")
  print(fit)
  }
sink()
#PROBLEM 22
# Viewing the output txt file, only an ARIMA 1,1,0 model was ever selected.
# Thus we conclude that bootstrapping an ARIMA process may not be a reliable way to identify it's parameters

#PROBLEM 23 Code

set.seed(1998852)
niter = 1000
estimates=matrix(0, nrow=niter, ncol=2)
for (iter in 1:niter){
  eps = rnorm(n+20)
  y = rep(0, n+20)
  for (t in 3:(n+20)){
    y[t] = .2825 *y[t-1] + 0.0570*y[t-2] + eps[t]
    }
  y = y[101:n+20]
  y = cumsum(y)
  y = ts(y, frequency=12)
  fit=arima(y, order=c(2,1,0))
  estimates[iter,] = fit$coef
  }

bias = colMeans(estimates) - oj_model$coef
MSE = c(var(estimates[ , 1]), var(estimates[ , 2])) + bias^2

#The MSE is computed as above