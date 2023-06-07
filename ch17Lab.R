setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list=ls())
dat = read.csv("Stock_Bond_2004_to_2006.csv", header = TRUE)
prices = dat[ , c(5, 7, 9, 11, 13, 15, 17, 24)]
n = dim(prices)[1]
dat2 = as.matrix(cbind(dat[(2:n), 3] / 365,
                       100 * (prices[2:n,] / prices[1:(n-1), ] - 1)))
names(dat2)[1] = "treasury"
risk_free = dat2[,1]
ExRet = dat2[ ,2:9] - risk_free
market = ExRet[ ,8]
stockExRet = ExRet[ ,1:7]
fit_reg = lm(stockExRet ~ market)
summary(fit_reg)
res = residuals(fit_reg)
pairs(res)
options(digits = 3)
betas = fit_reg$coeff[2, ]

## PROBLEM 1
# When reading the R output of the linear models, the Intercept for each stock is the alpha of the stock
# The slope with respect to market is the beta
# One by one, we see that all alphas have high p-values, and none are statistically significant
# Hence, we conclude that all alphas are zero. We fail to reject the null hypothesis

## PROBLEM 2
betas=fit_reg$coeff[2,]
betas * mean(market)
apply(stockExRet,2,mean)

#Check solutions
#The first line betas * mean(market) is computing estimates from the one factor model
# The second line is computing the estimates using sample means, which are dissimlar to each other
# The one factor model produces less variable estimates of mean returns

## PROBLEM 3
res = residuals(fit_reg)
options(digits=3)
cor(res) #computes correlation matrix of residuals.

#GM and F have a high correlation of 0.5091, there is moderate correlation between MRK and PFE (merck and pfizer) at 028
#The rest is kind of negligible
# GM and F are both auto companies, pfizer and merck are both pharmaceuticals

## PROBLEM 4
cov_capm = betas %*% t(betas) * var(market) + diag(diag(cov(res)))

## PROBLEM 5
# From calling summary(fit_reg), we look at UTX_AC and look at the multiple R squared, which is
# 0.357. Thus we conclude that the percentage of the excess return variance due to the market is
# 35.7%

## PROBLEM 6
est = 4*betas
# This value is in percent
est


# 17.9.1 Zero-beta Portfolios
library(linprog)
dat = read.csv("AlphaBeta.csv", header = TRUE)
weight_neg = -0.25
weight_pos = 0.25

#We want to do the following max cTx where c_T are the stocks alphas, x is the weights
# SUM_i: weight[i] * beta[i] = 0
# weight constraint: -0.25 <= weight[i] <= 0.25 for all i
# weight's are the decision variable
M = length(dat$alpha)
# The first M constraints are that wi ≤ B1 for all i, the
# next M constraints are that −B2 ≤ wi for all i, and the last constraint is that
# wT1 = 1.
B1 = 0.25 #positive boundary on weights
B2 = 0.25 #negative boundary
AmatLP1 = cbind(diag(1, nrow = M), matrix(0, nrow = M, ncol = M))
AmatLP2 = cbind(matrix(0, nrow = M, ncol = M), diag(1, nrow = M))
AmatLP3 = c(rep(1, M), rep(-1, M))
AmatLP4 = c(rep(1, M), rep(-1, M))
AmatLP = rbind(AmatLP1, AmatLP2, AmatLP3, AmatLP4)
bvecLP = c(rep(B1, M), rep(B2, M), 1, 0) #these are the constraints on vector b
cLP = c(dat$alpha, -dat$alpha)
const.dir = c(rep("<=", 2 * M), "=", "=")
resultLP_max = solveLP(cvec = cLP, bvec = bvecLP,
                          Amat = AmatLP, lpSolve = TRUE, maximum = TRUE)

# For the allocation vectors, call resultLP_min$solution
# The first M values (M=50) correspond to w_i <= B1, and the next M correspond to -B2 <= w_i
sum(resultLP_max$solution[1:50] > 0) #tells us how many we are going long
sum(resultLP_max$solution[51:100] > 0) # tells us how many we are going short
#To get the weights, correspond the index to the corresponding stock

#The alpha of the portfolio is resultLP_max$opt  = 0.266

#PROBLEM 8

#It makes sense we can't find a zero beta portfolio with too small a universe.
# For the linear optimization to work, we need a minimum of four long stocks based on our weight constraints
# But that is highly unlikely to zero out the beta.
