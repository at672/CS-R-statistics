setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list=ls())

library(Ecdat)
data(Irates)
r1 = Irates[,1]
n = length(r1)
lag_r1 = lag(r1)[-n]
delta_r1 = diff(r1)
n = length(lag_r1)
par(mfrow = c(3, 2))
plot(r1, main = "(a)")
plot(delta_r1, main = "(b)")
plot(delta_r1^2, main = "(c)")
plot(lag_r1, delta_r1, main = "(d)")
plot(lag_r1, delta_r1^2, main = "(e)")

start(Irates)
end(Irates)
time(Irates)[1:3]

#Problem 1:
#From calling View(Irates), we see that the 1st column contains the interest
# rates for a maturity of 1 month

# From calling start, end, and time(), we see that the sampling frequency is
# monthly, starting in December 1946 and ending in February 1991

# From looking at the R documentation: https://cran.r-project.org/web/packages/Ecdat/Ecdat.pdf
# Page 82 of pdf
# we see that the observation is from the country United States
# There are 531 observations.
# and the rates are expressed as percentages per year

#Problem 2:
# In the plot you have just created, panels (a), (b), and (c) show how the
# short rate, changes in the short rate, and squared changes in the short rate
# depend on time.

#The assumption that µ(t, r) = µ(r) appears valid based on plot (b)
# because b shows stationarity, i.e. a constant mean.
# The trend we see in plot d is that as time moves forward, the delta or change
# in r1 starts to become bigger in magnitude compared to its previous value


# CKLS (Chan, Karolyi, Longstaff, Sanders)
nlmod_CKLS = nls(delta_r1 ~ a * (theta-lag_r1),
                 start=list(theta = 5, a = 0.01),
                 control = list(maxiter = 200))
param = summary(nlmod_CKLS)$parameters[ , 1]
par(mfrow = c(2, 2))
t = seq(from = 1946, to = 1991 + 2 / 12, length = n)
plot(lag_r1, ylim = c(0, 16), ylab = "rate and theta",
     main = "(a)", type = "l")
abline(h = param[1], lwd = 2, col = "red")

#Problem 3: From the output of the nls call,
# we see theta converged to 5.32754
# and that a converged to 0.01984
# From the summary call, we see the values of their standard error, and we know that
# The confidence interval is
# param +/- z_alpha/2 * standard(error)
hw = 1.96*summary(nlmod_CKLS)$parameters[ , 2]
#not the most elegant way, but too lazy to make it neater.
CI = c(param-hw, param+hw)

#Problem 4 set up
#A = σ^2
#B = 2*gamma
res_sq = residuals(nlmod_CKLS)^2
nlmod_CKLS_res <- nls(res_sq ~ A*lag_r1^B,
                      start = list(A = 0.2, B = 1/2))
param2 = summary(nlmod_CKLS_res)$parameters[ , 1]
plot(lag_r1, sqrt(res_sq), pch = 5, ylim = c(0, 6),
     main = "(b)")
attach(as.list(param2))
curve(sqrt(A * x^B), add = T, col = "red", lwd = 3)

#Since sigma(t,r) = sigma*r^gamma by our assumption
# sigma(t,r)^2 = sigma^2 * r^2*gamma, i.e. our A and B
#Thus, we take the square root of A, and divide B by 2 to get sigma and gamma
param2 = c(sqrt(param2[1]), param2[2]/2)
se = summary(nlmod_CKLS_res)$parameters[ , 2]
se = c(sqrt(se)[1], se[2]/2)
hw2 = 1.96*se
CI2 = c(param2-hw2, param2+hw2)


#Problem 5 set up
nlmod_CKLS_wt = nls(delta_r1 ~ a * (theta-lag_r1),
                    start = list(theta = 5, a = 0.01),
                    control = list(maxiter = 200),
                    weights = 1 / fitted(nlmod_CKLS_res))
plot(lag_r1, ylim = c(0, 16), ylab = "rate and theta",
     main = "(c)", type = "l")
param3 = summary(nlmod_CKLS_wt)$parameters[ , 1]
abline(h = param3[1], lwd = 2, col = "red")

# The unweighted estimate of theta is higher, at 5.327
# the weighted estimate of theta is lower, at 2.21
# Weighted least squares is being performed with the inverse of the square of the volatility/residuals
# This is a pretty common weighting scheme
# This is meant to penalize values that contain higher volatility
# We can see visually the process becomes more volatile as time goes on,
# So the mean in that region is not given as much weight. Hence, the entire
# estimate is dragged down

#Problem 6

library(AER)
data(HousePrices)
fit1 = lm(price ~ ., data = HousePrices)
summary(fit1)
library(MASS)
par(mfrow = c(1, 1))
fit2 = boxcox(fit1, xlab = expression(alpha))

#fit2 returns two numerics of length 100, the x and y points that are used to plot the profile log likliehood
# We need the max of y and find the corresponding x value

alpha = fit2$x[fit2$y==max(fit2$y)]

ind2 = (fit2$y > max(fit2$y) - qchisq(0.95, df = 1) / 2)
CI_alpha = fit2$x[ind2]
#the MLE of alpha is 0.14 and it's 95% CI is from 0.02 to 0.2626

#Problem 7
library(car)
alphahat = alpha
fit3 = lm(bcPower(price, alphahat) ~ ., data = HousePrices)
summary(fit3)
AIC(fit1)
AIC(fit3)

# Because of the huge decrease in AIC, we are confident that fit3 is superior
# to fit1.

#Problem 8
#NO. Because the the data are cross sectional and are in no particular order.

#Problem 9

library(AER)
data(HousePrices)
fit1 = glm(aircon ~ ., family = "binomial",
           data = HousePrices)
summary(fit1)
library(MASS)
fit2 = stepAIC(fit1)
summary(fit2)

# The step AIC function got rid of full basement, then bedrooms
# then recreation, then garage,
# then lotsize, then driveway
# then prefer
# then it stopped. 
# Thus, the most useful variables are the remaining ones.
# price, bathroom, stories, gasheatyes
# Qualitatively, we see that the slopes are positive for price, and for stories
# meaning that multi-story houses and expensive houses are likely to have AC
# the slopes are negative for gas heat yes and bathrooms, meaning that houses 
# with gas heat and more bathrooms are less likely to have AC
# if you look closer, we see bathrooms is significant only at the 90% level
# also, its std.error is the same magnitude as its intercept
# so while the value for bathrooms seems counterintuitive, it's not a strong negative
# correlation.

#Problem 10
#Probability with bathrooms
prob_fit2 = plogis( c(1, 42000,1, 2, 0) %*% coef(fit2))

#From the solutions, if you remove bathrooms, and then compute the probability,
# you get 0.133