setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list=ls())
library(AER)
data("USMacroG")
MacroDiff = as.data.frame(apply(USMacroG, 2, diff))
attach(MacroDiff)
pairs(cbind(consumption, dpi, cpi, government, unemp))

#Problem 1: We don't observe any significant outliers.
# The variables best suited to observe a change in consumption are dpi and unemp.
# We see a good positive correlation with dpi, and a weak negative correlation
# with unemp
# Because the correlation between the other four variables (dpi, cpi, government, unemp)
# are all low, colinearity should not be a major concern

#Problem 2: 

fitLm1 = lm(consumption ~ dpi + cpi + government + unemp)
summary(fitLm1)
confint(fitLm1)
# 
# Call:
#   lm(formula = consumption ~ dpi + cpi + government + unemp)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -60.626 -12.203  -2.678   9.862  59.283 
# 
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  14.752317   2.520168   5.854 1.97e-08 ***
#   dpi           0.353044   0.047982   7.358 4.87e-12 ***
#   cpi           0.726576   0.678754   1.070    0.286    
# government   -0.002158   0.118142  -0.018    0.985    
# unemp       -16.304368   3.855214  -4.229 3.58e-05 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 20.39 on 198 degrees of freedom
# Multiple R-squared:  0.3385,	Adjusted R-squared:  0.3252 
# F-statistic: 25.33 on 4 and 198 DF,  p-value: < 2.2e-16
# 
# 
# 2.5 %     97.5 %
#   (Intercept)   9.7825024 19.7221311
# dpi           0.2584219  0.4476652
# cpi          -0.6119382  2.0650912
# government   -0.2351363  0.2308197
# unemp       -23.9069164 -8.7018187


# From the summary, the variables dpi and unemp seem useful for predicting changes in consumption. 
# We know this because of the t-values and Pr(>|t|).
# They are statistically significant
# Large t-values on the other hand are present for cpi and government, so they aren't too useful

#Problem 3
#ANOVA: Analysis of Variance
anova(fitLm1)
# No, the ANOVA table doesn't provide anything more useful. The F values suggest
# to choose dpi and unemp

#Problem 4
library(MASS)
fitLm2 = stepAIC(fitLm1)
summary(fitLm2)
# From the output we see that the variables government and cpi are removed, and in that order
# 

#Problem 5
AIC(fitLm1)
AIC(fitLm2)
AIC(fitLm1) - AIC(fitLm2)

#The improvement in AIC is 2.827648. This improvement is not large in terms of AIC.
# Let's think about this. Dropping variables means the log-likelihood increases
# which means the AIC increases. But it also means the number of variables is going down
# which decreases AIC. The decrease due to dropping variables is limited.
# If there are many varialbes, a huge decrease in AIC is possible if a very large number of variables can be dropped.

#Problem 6
library(car)
vif(fitLm1)
vif(fitLm2)

#There was little colinearity in the original model since all four VIFs are near their lower bound of 1.
# Since the colinearity was little to begin with, it cannot be lowered substantially.


#Problem 7
par(mfrow = c(2, 2))
sp = 0.8
crPlot(fitLm1, dpi, span = sp, col = "black")
crPlot(fitLm1, cpi, span = sp, col = "black")
crPlot(fitLm1, government, span = sp, col = "black")
crPlot(fitLm1, unemp, span = sp, col = "black")

#Conclusions we can draw:
# CPI and government have near horizontal lines, meaning that there is little correlation for those 2 variables
# this means our thesis that they can be dropped is supported

# Because the lowess curves are close to the least squares line, it indicates
# that the effects of dpi and unemp on consumption are linear

