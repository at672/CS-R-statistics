setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list=ls())
library("fGarch")
bmwRet = read.csv("bmwRet.csv")
n = dim(bmwRet)[1]

kurt = kurtosis(bmwRet[ ,2], method = "moment")
skew = skewness(bmwRet[,2], method = "moment")
fit_skewt = sstdFit(bmwRet[ ,2])

q.grid = (1:n) / (n+1)
qqplot(bmwRet[ ,2], qsstd(q.grid, fit_skewt$estimate[1],
                             fit_skewt$estimate[2],
                             fit_skewt$estimate[3], fit_skewt$estimate[4]), ylab = "skewed-t quantiles" )
abline(lm(qsstd(
  p = c(0.25, 0.75),
  mean = fit_skewt$estimate[1],
  sd = fit_skewt$estimate[2],
  nu = fit_skewt$estimate[3],
  xi = fit_skewt$estimate[4]) ~
            quantile(bmwRet[ ,2], c(0.25, 0.75))), col = 'red', lwd = 2)
title(main = "QQ Plot of BMW Returns")

#The MLE of nu is 2.988
#With this estimate, the kurtosis and skew are both infinite. For finite skew you need nu > 3, and for finite
#kurtosis you need nu > 4

#Problem 2: PLots quantile kurtosis as a function of degrees freedom.
p1 = 0.025
p2 = 0.25
nu = seq(1, 10, 0.25)
numerator = qt(1-p1,df=nu)-qt(p1,df=nu)
denominator = qt(1-p2,df=nu)-qt(p2,df=nu)
plot(nu,numerator/denominator,ylab=expression(paste("quKurt(",nu,")")),
     xlab=expression(nu))

quKurt = function(y, p1 = 0.025, p2 = 0.25)
{
  Q = quantile(y, c(p1, p2, 1 - p2, 1 - p1))
  k = (Q[4] - Q[1]) / (Q[3] - Q[2])
  k
}
nboot = 5000
ModelFree_kurt = rep(0, nboot)
ModelBased_kurt = rep(0, nboot)
set.seed("5640")
for (i in 1:nboot)
{
  samp_ModelFree = sample(bmwRet[,2], n, replace = TRUE)
  samp_ModelBased = rsstd(n, fit_skewt$estimate[1],
                          fit_skewt$estimate[2],
                          fit_skewt$estimate[3], fit_skewt$estimate[4])
  ModelFree_kurt[i] = quKurt(samp_ModelFree)
  ModelBased_kurt[i] = quKurt(samp_ModelBased)
}
#Problem 3:
par(mfrow = c(1,2))
plot(density(ModelFree_kurt))
plot(density(ModelBased_kurt))
par(mfrow = c(1,1))
boxplot(cbind(ModelFree_kurt,ModelBased_kurt))
#Problem 3: The model free and model based distributions are similar. Both are relatively symmetric and light-tailed
#The boxplots are better suited to compare the two samples. From those we see that the model free kurtosis has
# a higher mean and higher variance

#Problem 4:
options(digits =4)
quantile(ModelFree_kurt, c(0.05, 0.95))
quantile(ModelBased_kurt, c(0.05, 0.95))

#Problem 5
library(bootstrap)
bca = bcanon(bmwRet[,2],5000,quKurt)
bca$confpoints

#The 90% confidence interval can be observed from the output. It is from [2,]=0.05 alpha to [7,]=0.95 alpha
#in this case, the o utput shows 4.115 to 4.502.
#The CI is shifted to the right because the BCa interval corrects for bias asnd for the variance of the estimator
#Percentile intervals do not make this correction


#Problem 6 code
library(bootstrap)
#Kurtosis = function(x) mean(((x - mean(x)) / sd(x))^4)
Kurtosis = quKurt
set.seed(3751)
niter = 500
nboot = 400

n = 50
nu = 10
trueKurtosis = 3 + 6 / (nu - 4)
correct = matrix(nrow = niter, ncol = 5)
width = matrix(nrow = niter, ncol = 5)
error = matrix(nrow = niter, ncol = 1)
t1 = proc.time()
for (i in 1:niter){
  y = rt(n,nu)
  int1 = boott(y, Kurtosis, nboott = nboot,
               nbootsd = 50)$confpoints[c(3, 9)]
  width[i,1] = int1[2] - int1[1]
  correct[i,1] = as.numeric((int1[1] < trueKurtosis) &
                              (trueKurtosis < int1[2]))
  int2 = bcanon(y, nboot, Kurtosis)$confpoints[c(1, 8), 2]
  width[i,2] = int2[2] - int2[1]
  correct[i,2] = as.numeric((int2[1] < trueKurtosis) &
                              (trueKurtosis < int2[2]))
  boot = bootstrap(y, nboot, Kurtosis)$thetastar
  int3 = Kurtosis(y) + 1.96 * c(-1, 1) * sd(boot)
  width[i,3] = int3[2] - int3[1]
  correct[i,3] = as.numeric((int3[1] < trueKurtosis) &
                              (trueKurtosis < int3[2]))
  int4 = quantile(boot, c(0.025, 0.975))
  width[i,4] = int4[2] - int4[1]
  correct[i,4] = as.numeric((int4[1] < trueKurtosis) &
                              (trueKurtosis < int4[2]))
  int5 = 2*Kurtosis(y) - quantile(boot, c(0.975, 0.025))
  width[i,5] = int5[2] - int5[1]
  correct[i,5] = as.numeric((int5[1] < trueKurtosis) &
                              (trueKurtosis < int5[2]))
  error[i] = mean(boot) - Kurtosis(y)
}
t2 = proc.time()
(t2 - t1)/60
colMeans(width)
colMeans(correct)
options(digits = 3)
mean(error)
mean(error^2)

#Output:
# > (t2 - t1)/60
# user      system     elapsed 
# 4.460333333 0.007333333 4.468000000 
# > colMeans(width)
# [1] 5.390828 2.971474 2.592597 2.484077 2.484077
# > colMeans(correct)
# [1] 0.678 0.686 0.530 0.558 0.492
# > options(digits = 3)
# > mean(error)
# [1] -0.17
# > mean(error^2)
# [1] 0.141


#Problem 6: In order, the five bootstrap intervals are:
# 1) the 0.025 and 0.975 values, i.e. the 95% conf interval
# 2) the 0.001 and 0.999 values, i.e. the 99.95% conf interval
# 3) int3 is the 9% confidence interval it appears
# 4) int4 appears to be the 95% interval, determined from quantiles
# 5) int5 is also the 9% interval, determined from quantiles, albeit differently than 4

#Problem 7
#The value of B is the number of resamples i.e. nboot in the code, 400.

#Problem 8: 500 simulations are used

#Problem 9: The bias estimate is the expected value of the kurtosis
# minus the actual value of the kurtosis. This is the variable "mean(error)" in the code
# 

#Problem 10:
#MSE = Bias^2 + se_boot^2 i.e. mean(error^2)

#Problem 11:
#Actual coverage probability
#colMeans(width) tells us the average over the simulations of the confidence interval width 
# we determined from our nboot bootstraps. IDX[2] tells us the BCa width, and IDX[1]
# tells us the bootstrap-t interval width


#Problem 12:
#The variable correct is a probability determining whether or not the true kurtosis actually lies in the bounds
# determined from the boostrapping. These probabilites are all lower than 95/99%, suggesting
# that estimating the kurtosis is difficult
# We know the width of the interval, but we don't know the midpoint (the value itself).
# WE know:
# error[i] = mean(boot) - Kurtosis(y)
# where mean(error) results in the average bias seen across simulations
# thus, true + error = estimate, and we know true kurtosis Kurtosis(y) # quantile based in problem 15
# so the CI has midpoint Kurtosis(y) + mean(error)
# and has the width specified by idx[2] in colMeans(width)

#Problem 13:
#I would not consider any intervals as highly accurate. Though if you run this for many more simulations that could change.
#I only did the bare minimum


#Problem 14:
#The clock time is (t2-t1)/60. About 5 minutes for true kurtosis, about 20 for quantile kurtosis


#Problem 15:
# quantile based:
#   > (t2 - t1)/60
# user    system   elapsed 
# 19.225833  0.002333 19.231167 
# > colMeans(width)
# [1] 2.861 2.976 2.850 2.761 2.761
# > colMeans(correct)
# [1] 0.650 0.740 0.660 0.776 0.476
# > options(digits = 3)
# > mean(error)
# [1] 0.0597
# > mean(error^2)
# [1] 0.0607


#Quantile based kurtosis has lower MSE, lower bias, and generally speaking has more accuracy.
# The most accurate interval is the fourth, which is also the tightest.


