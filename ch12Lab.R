setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list=ls())

TbGdpPi = read.csv("TbGdpPi.csv", header=TRUE)
# r = the 91-day treasury bill rate
# y = the log of real GDP
# pi = the inflation rate
TbGdpPi = ts(TbGdpPi, start = 1955, freq = 4)
library(tseries)
plot(TbGdpPi)
acf(TbGdpPi)
adf.test(TbGdpPi[,1])
adf.test(TbGdpPi[,2])
adf.test(TbGdpPi[,3])

#Problem 1:
# a) Visually, it's hard to tell if there is any mean-reversion. There are no clear signs
# we know that y looks very nonstationary
# The ACF plots show nearly linear decays for r and y, and less so for pi
# this also suggests that the series are not stationary

# b) ADF test: null hypothesis is unit-root nonstationarity. If we look at the p-values,
# We see that the first two have large p-values, and the the last one, pi is
# significant at the 10% level. That means we fail to reject the null hypothesis.
# We conclude that all 3 are nonstationary series.

#Problem 2 set up

diff_rate = diff(TbGdpPi)
adf.test(diff_rate[,1])
adf.test(diff_rate[,2])
adf.test(diff_rate[,3])
pairs(diff_rate) # scatterplot matrix
plot(diff_rate) # time series plots
acf(diff_rate) # auto- and cross-correlations

#Problem 2:
# First, we analyze the plots. The scatter plots show a fair amount of dispersion for all plots
# We do see some slight of tail dependence on the ry plot and rpi plot
# The time seriesplots look somewhat stationary, wih some volatility clustering here and there.
# They look stationary because we can see mean reversion
# The autocorrelation plots seem to be within the bounds mostly for r and y with the exception of the lag < 1
# The acf of pi seems to be out of the dashed lines

# 1. According to the ADF tests, the differenced series do seem to be stationary. The p-values are all less than 0.01
# Meaning we have statistical significance at the 99% level. We reject the null hypothesis.

# 2. Yes, we do see some evidence of autocorrelations in the differenced series for r and y with lag < 1, 
# and for pi (lag 0 to 4)
# A rapid decay to 0 --> evidence of stationarity
# pi, the inflation rates, shows strong autocorrelations at multiples of 1 year due to seasonality


# Problem 3 set up
par(mfrow=c(1,1))
boxplot(diff_rate[,1] ~ cycle(diff_rate))
#This plots quarterly  periods of each observations

#What we notice is that while the medians are relatively equal,
# the quartiles are larger on index 4, and the min max values are tightest on idx 1
# We conclude that the means are the same, statistically


library(forecast)
auto.arima(TbGdpPi[,1], max.P=0, max.Q=0, ic="aic")

#Problem 4:
# 1. First order differencing is chosen. Does it agree with previous conclusions: i don't know
# 2. AIC chose an ARIMA(0, 1, 3) or MA(3) model after first difference
# 3. the Goodness of fit criterion used is log-likelihood ?

auto.arima(TbGdpPi[,1], max.P=0, max.Q=0, ic="bic")

# 4. the model does not change when changing to BIC
# 5. in mathematical notation: Delta Yt = theta1 epst-1 + theta2 eps_t-2 + theta3 eps_t-3 + eps_t

# His solutions have different results, interesting.

fit1 = arima(TbGdpPi[,1], order=c(0,1,3))
acf(residuals(fit1))
#fitdf is sum of p and q parameters.
Box.test(residuals(fit1), lag = 12, type="Ljung", fitdf=3)

#Problem 5: The ACF plot shows that most auto correlations are within the bound.
# The Box test has a p-value that's low, significant at the 98% level. Thus there is
# statistically significant residual autocorrelation.

#Problem 6 set up
resid2 = (residuals(fit1) - mean(residuals(fit1)))^2
acf(resid2)
Box.test(resid2, lag = 12, type="Ljung")

#There is strong evidence to reject the null hypothesis. A serial correlation is present in resid2,
# Since resid2 are the squared residuals (mean-centered), we conclude there are GARCH effects

#12.15.2 Forecasting
# Problem 7 set up

TbGdpPi = read.csv("TbGdpPi.csv", header=TRUE)
attach(TbGdpPi)
# r = the 91-day treasury bill rate
# y = the log of real GDP
# pi = the inflation rate
# fit the non-seasonal ARIMA model found by auto.arima()
# quarterly observations from 1955-1 to 2013-4
year = seq(1955,2013.75, by=0.25)
library(forecast)
auto.arima(pi, max.P=0, max.Q=0, ic="bic")
fit = arima(pi, order=c(0,1,1))
forecasts = predict(fit, 36)
plot(year,pi,xlim=c(1980,2023), ylim=c(-7,12), type="b")
lines(seq(from=2014, by=.25, length=36), forecasts$pred, col="red")
lines(seq(from=2014, by=.25, length=36),
         forecasts$pred + 1.96*forecasts$se, col="blue")
lines(seq(from=2014, by=.25, length=36),
         forecasts$pred - 1.96*forecasts$se, col="blue")

#Problem 7
# 1. The blue curves widen because as you move into the future, there is more uncertainty
# 2. The predictions are constant because the forecasts revert quickly to the series mean
