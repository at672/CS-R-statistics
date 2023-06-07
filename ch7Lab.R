setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list=ls())
berndtInvest = read.csv("berndtInvest.csv")
Berndt = as.matrix(berndtInvest[, 2:5])
cov(Berndt)
cor(Berndt)
pairs(Berndt)

#Problem 1: Compute Var(0.5X1 + 0.3X2 + 0.2X3)
# = 0.5^2Var(x1) + 0.3^2var(x2) + 0.2^2var(x3)
# + 2*[0.5*0.3 cov(x1, x2) + 0.5*0.2*cov(x1, x3) + 0.3*0.2*cov(x2, x3)]

#In matrix notation, Var(aX)= a*Var(X)*a_T, where a is a row vector, Var(X)
# is a matrix, and a_T denotes the transpose
#Thus,
a = matrix( c(0.5, 0.3, 0.2), nrow = 1, ncol = 3)
#%*% is matrix algebra in R
Prob1 = a %*% cov(Berndt)[1:3,1:3] %*% t(a)
#The answer is the quantity obtained above, 0.004408865

#Problem 2:
library(MASS) # needed for cov.trob
library(mnormt) # needed for dmt
df = seq(2.5, 8, 0.01)
n = length(df)
loglik_profile = rep(0, n)
for(i in 1:n)
{
  fit = cov.trob(Berndt, nu = df[i])
  mu = as.vector(fit$center)
  sigma = matrix(fit$cov, nrow = 4)
  loglik_profile[i] = sum(log(dmt(Berndt, mean = fit$center,
                                  S= fit$cov, df = df[i])))
}

#max MLE:
MLE_value = loglik_profile[which.max(loglik_profile)]
MLE_df = df[which.max(loglik_profile)]

#Create LOGICAL vectors representing where the condition is satisfied
#ind is for MLE, ind2 is for CI
ind = (loglik_profile == max(loglik_profile))
ind2 = (loglik_profile > max(loglik_profile) - qchisq(0.90, df = 1) / 2)

#Construct the confidence interval. Find it's limits first, then join it in a vector
LL_CI_values = loglik_profile[ind2]
right_limit = LL_CI_values[length(LL_CI_values)]
left_limit = LL_CI_values[1]
right_limit_df = df[length(LL_CI_values)]
left_limit_df = df[1]

CI = c(left_limit, right_limit)

#Generate Plot
plot(x=df, y=loglik_profile, type = 'l')
title('Log Likelihood Profile')
points(MLE_df, MLE_value, col = 'red', lwd = 2)
lines(x=df[ind2], y = LL_CI_values, col = 'blue', lwd = 2)


ys = as.numeric( c( left_limit, right_limit) )
xs = as.numeric( c( left_limit_df, right_limit_df) )
fit = lm(ys~xs)
abline(fit, col = 'green')

#Explanation of the plot. The red circle is the MLE. All points in blue are in the MLE.
#The green line represents the confidence level on the plot. Anything above it is in our CI, anything below isn't.
#The MLE of nu is MLE_df = 4.55
#The value of the Log likelihood function for 4.55 df is MLE_value, or 539.179

#7.13.2
library(MASS) # need for mvrnorm
#par(mfrow=c(1,4))
par(mfrow=c(2,2))

N = 2500
nu = 3

set.seed(5640)
cov=matrix(c(1, 0.8, 0.8, 1), nrow = 2)
x= mvrnorm(N, mu = c(0, 0), Sigma = cov)
w = sqrt(nu / rchisq(N, df = nu))
x = x * cbind(w, w)
plot(x, main = "(a)")

set.seed(5640)
cov=matrix(c(1, 0.8, 0.8, 1),nrow = 2)
x= mvrnorm(N, mu = c(0, 0), Sigma = cov)
w1 = sqrt(nu / rchisq(N, df = nu))
w2 = sqrt(nu / rchisq(N, df = nu))
x = x * cbind(w1, w2)
plot(x, main = "(b)")

set.seed(5640)
cov=matrix(c(1, 0, 0, 1), nrow = 2)
x= mvrnorm(N, mu = c(0, 0), Sigma = cov)
w1 = sqrt(nu / rchisq(N, df = nu))
w2 = sqrt(nu / rchisq(N, df = nu))
x = x * cbind(w1, w2)
plot(x, main = "(c)")

set.seed(5640)
cov=matrix(c(1, 0, 0, 1), nrow = 2)
x= mvrnorm(N, mu = c(0, 0), Sigma = cov)
w = sqrt(nu / rchisq(N, df = nu))
x = x * cbind(w, w)
plot(x, main = "(d)")

#Problem 3, 4, and 5:
# There are four classifications:
# Correlated, tail dependence
# Correlated, no tail dependence
# Uncorrelated, tail dependence
# Uncorrelated, no tail dependence

# It is easy to see whether or not the distribution is correlated or not by observing
# the value of the covariance matrix in the mvrnorm command
#Thus, Plot (c) and (d) do not have correlated samples, which seem right based on the plots
# Tail dependence: If returns tend to occur on the corners, that means that the plot 
# exhibits tail dependence. This means that extreme values for one of the datasets (e.g. stock 1)
# indicates that the other dataset (e.g. stock 2) would also have an extreme value
# If the returns tend to be on the axes, then it shows tail independence.

# With this in mind, it's clear that (a) shows tail dependence, (b) and (c) do not. 
# D shows tail dependence, albeit its harder to see from the plot because the uncorrelation of the values
# adds considerable dispersion

#Thus,
# Problem 3: C and D have independent variates
# P4: Correlated but no tail dependence is (b)
# P5: Uncorrelated with tail dependence is D

#Problem 6
nu = 5
mu = as.matrix(c(0.001, 0.002))
#Sigma here is the covariance matrix
Sigma = matrix( c(0.1, 0.03, 0.03, 0.15), nrow = 2, ncol = 2)
#R = (X+Y)/2
Scale = (nu-2)/nu * Sigma
r_var = t(as.matrix(c(0.5, 0.5))) %*% Sigma %*% as.matrix(c(0.5, 0.5))
#Problem 6 part a: If w is a vector of weights, and Y has a m.v. t distr,
#then w_T * Y has a univariate T distribution with mean wTmu, and variance w& Sigma w
#Thus R = (X+Y)/2 has a univariate t distribution with the same degrees freedom
# mean 0.5 mean(x) + 0.5 mean (y) = 0.0015
# and variance computed above as r_var: 0.0775

#Part b

Samples = rmt(n = 10000, mean = t(mu), S = Scale, df = nu)
R = 0.5*(Samples[,1] + Samples[,2])
R_quantiles = quantile(x = R, probs = c(0.01, 0.05, 0.95, 0.99))
# The upper quantile is R_quantiles[4]


#Problem 7 code:
library(mnormt)
data(CRSPday, package = "Ecdat")
Y = CRSPday[ , c(5, 7)]
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

#Problem 7:
# a: A = chol(cov(Y)), computes the cholesky decomposition of the covariance matrix of Y, Y is the CRSP data
# The cholesky decomposition is used to to get a upper triangular matrix which we can supply an initial parameter guess
# into the optimization function

# b: By calling fit_mvt$par, we get the MLE of the 6 parameter set.
# idx 1, 2 is the mean vect, 3, 4, 5 is the A matrix values, and 6 is df

# c: fit_mvt$hessian gives us the hessian matrix. The fisher information matrix is the hessian matrix
# 

# d: 
fisher = fit_mvt$hessian
se = sqrt(diag(solve(fisher)))

# e: MLE of covariance matrix of returns:
ML = fit_mvt$par
Ahat = matrix(c(ML[3:4],0,ML[5]),nrow=2, byrow = TRUE)
COV_Y = t(Ahat)%*%Ahat * ML[6]/(ML[6]-2)
#compare this to cov(Y). We see they are very close.
cov(Y)
COV_Y

#f: MLE of rho:
rho = COV_Y[1, 2]/sqrt(COV_Y[1,1]*COV_Y[2,2])
rho
cor(Y)
#we see the values are similar.