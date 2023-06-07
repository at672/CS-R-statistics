setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list=ls())
library(AER)
data(CPS1988)
attach(CPS1988)
fitLm1 = lm(wage ~ education + experience + ethnicity)

#par(mfrow = c(3, 2))
resid1 = rstudent(fitLm1)
plot(fitLm1$fit, resid1,
     ylim = c(-1500, 1500), main = "(a)")
lines(lowess(fitLm1$fit, resid1, f = 0.2), lwd = 5, col = "red")
abline(h = 0, col = "blue", lwd = 5)
plot(fitLm1$fit, abs(resid1),
     ylim = c(0, 1500), main = "(b)")
lines(lowess(fitLm1$fit, abs(resid1), f = 0.2),
      lwd = 5, col = "red")
abline(h = mean(abs(resid1)), col = "blue", lwd = 5)

qqnorm(resid1, datax = FALSE, main = "(c)")
qqline(resid1, datax = FALSE, lwd = 5, col = "blue")
plot(education, resid1, ylim = c(-1000, 1500), main = "(d)")
lines(lowess(education, resid1), lwd = 5, col = "red")
abline(h = 0, col = "blue", lwd = 5)
plot(experience, resid1, ylim = c(-1000, 1500), main = "(e)")
lines(lowess(experience, resid1), lwd = 5, col = "red")
abline(h = 0, col = "blue", lwd = 5)

#Problem 1: Read the solutions, better explanation than what's written here.
# Plot a is a residual plot of the residuals of the linear regression model
# Plot b is an absolute value residual plot.
# Plot c is a QQplot of the residuals.
# Plot d is a plot of residuals agains education, one of the regressors
# Plot e is a plot of residuals against experience, another regressor.

#If you run the code for the plots one at a time, you see that the blue lines and red lines
# are on top of each other

# The most obvious thing we see is in plot c. As the theoretical quantiles go up,
# The values in the sample quantiles go up even faster. This is a sign of 
# Heavy tailed behavior on the positive end.
# This is a sign of substantial hetereoskedasticity.


#Problem 2
fitLm2 = lm(log(wage) ~ education + experience + ethnicity)

resid2 = rstudent(fitLm2)
plot(fitLm2$fit, resid2,
     ylim = c(-1500, 1500), main = "(a)")
lines(lowess(fitLm2$fit, resid2, f = 0.2), lwd = 5, col = "red")
abline(h = 0, col = "blue", lwd = 5)
plot(fitLm2$fit, abs(resid2),
     ylim = c(0, 1500), main = "(b)")
lines(lowess(fitLm2$fit, abs(resid2), f = 0.2),
      lwd = 5, col = "red")
abline(h = mean(abs(resid2)), col = "blue", lwd = 5)

qqnorm(resid2, datax = FALSE, main = "(c)")
qqline(resid2, datax = FALSE, lwd = 5, col = "blue")
plot(education, resid2, ylim = c(-1000, 1500), main = "(d)")
lines(lowess(education, resid2), lwd = 5, col = "red")
abline(h = 0, col = "blue", lwd = 5)
plot(experience, resid2, ylim = c(-1000, 1500), main = "(e)")
lines(lowess(experience, resid2), lwd = 5, col = "red")
abline(h = 0, col = "blue", lwd = 5)

#We see a much better qqplot

#Problem 3
par(mfrow = c(3, 2))
fitLm3 = lm(log(wage) ~ poly(education, 2) + poly(experience, 2) + ethnicity)
resid3 = rstudent(fitLm3)
plot(fitLm3$fit, resid3,
     ylim = c(-1500, 1500), main = "(a)")
lines(lowess(fitLm3$fit, resid3, f = 0.2), lwd = 5, col = "red")
abline(h = 0, col = "blue", lwd = 5)
plot(fitLm3$fit, abs(resid3),
     ylim = c(0, 1500), main = "(b)")
lines(lowess(fitLm3$fit, abs(resid3), f = 0.2),
      lwd = 5, col = "red")
abline(h = mean(abs(resid3)), col = "blue", lwd = 5)
qqnorm(resid3, datax = FALSE, main = "(c)")
qqline(resid3, datax = FALSE, lwd = 5, col = "blue")
plot(education, resid3, ylim = c(-1000, 1500), main = "(d)")
lines(lowess(education, resid3), lwd = 5, col = "red")
abline(h = 0, col = "blue", lwd = 5)
plot(experience, resid3, ylim = c(-1000, 1500), main = "(e)")
lines(lowess(experience, resid3), lwd = 5, col = "red")
abline(h = 0, col = "blue", lwd = 5)


#Problem 4
#Read solution

#Problem 5
# Read Solution
