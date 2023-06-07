setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list=ls())
yieldDat = read.table("yields.txt", header = T)
maturity = c((0:5), 5.5, 6.5, 7.5, 8.5, 9.5)
pairs(yieldDat)
par(mfrow = c(4,3))
for (i in 0:11)
{
  plot(maturity, yieldDat[100 * i + 1, ], type = "b")
}
## Compute Eigenvalues and eigenvectors of sample cov matrix
eig = eigen(cov(yieldDat))
eig$values
eig$vectors
par(mfrow = c(1, 1))
barplot(eig$values)

#Plot first four eigenvectors

par(mfrow=c(2, 2))
plot(eig$vector[ , 1], ylim = c(-0.7, 0.7), type = "b")
abline(h = 0)
plot(eig$vector[ , 2], ylim = c(-0.7, 0.7), type = "b")
abline(h = 0)
plot(eig$vector[ , 3], ylim = c(-0.7, 0.7), type = "b")
abline(h = 0)
plot(eig$vector[ , 4], ylim = c(-0.7, 0.7), type = "b")
abline(h = 0)

## Problem 1
# Plot first column
par(mfrow=c(1, 1))
plot(yieldDat[ ,1])


## The plot appears to be nonstationary. 
## PROBLEM 2
library("tseries")
adf.test(yieldDat[ , 1])
#The results of the ADF test show a p-value of 0.9249.
# With such a high p-value we fail to reject the null hypothesis, concluding that it is indeed non-stationary

## PROBLEM 3
n=dim(yieldDat)[1]
#Recall that in R, negative indexing means exclude that index. I.e. use all indexes except that one
delta_yield = yieldDat[-1, ] - yieldDat[-n, ]
plot(delta_yield[ ,1])
adf.test(delta_yield[ , 1])

#Yes, the plot appears to be a stationary process, and the p-value for the ADF test is very low, so we conclude that
# The process is indeed stationary


## PROBLEM 4
pca_del = princomp(delta_yield)
names(pca_del)
summary(pca_del)
par(mfrow = c(1, 1))
plot(pca_del)

# I)
# sdev is the standard deviation. Specifically, it is the square root
# of the eigenvalues of the covariance matrix.
# We can see this with the following code.
# note the scaling of(n-1)/n since princomp uses n and not n-1 as the sample size for sample cov
options(digits=3,width=60)
eig = eigen(cov(delta_yield)*((n-1)/n))
sqrt(eig$values)
# II) Loadings are the eigenvectors of the covariance matrix. It is ok if one of these is positive and the other is negative
# Vector-wise, they are the same
eig$vectors[,1]
pca_del$loadings[,1]

# III) center is the mean vector.
colMeans(delta_yield)
pca_del$center

# IV) scores is the projections of the yields minus their means onto the eigenvectors.
mu = colMeans(delta_yield)
S = as.matrix(delta_yield-mu) %*% eig$vector
S[1,]
pca_del$scores[1,]

## PROBLEM 4 b)
# The call eig$vectors[, 1:2] gives us the first two eigenvectors of the cov matrix
eig$vectors[, 1:2]

## PROBLEM 4 c)
# First find out where the maximum eigen value occurs. This should be the first eigenvector, I.E. the PRINCIPAL component
eig$values == max(eig$values)
#Thus, we have our answer

## PROBLEM 4 d)
#From reading the output summary(pca_del), we see that PC1 has 87.6% of the total variation. PC2 has an additional 7.97%
# bringing the cumulative variation up to 95.56%. Thus we need 2 PCs in order to explain at least 95%


## PROBLEM 5
# 18.8.2 Fitting Factor Models by Time Series Regression

# Uses daily data 2004-2005

FF_data = read.table("FamaFrenchDaily_2004_to_2005.txt", header = TRUE)
#FF_data = read.table("FamaFrenchDaily_2004_to_2006.txt", header = TRUE)
FF_data = FF_data[-1, ] # delete first row since stocks_diff

stocks = read.csv("Stock_FX_Bond_2004_to_2005.csv",header=T)
#stocks = read.csv("Stock_Bond_2004_to_2006.csv",header=T)
attach(stocks)
stocks_subset = as.data.frame(cbind(GM_AC, F_AC, UTX_AC, MRK_AC))
stocks_diff = as.data.frame(100 * apply(log(stocks_subset),
                                        2, diff) - FF_data$RF)
names(stocks_diff) = c("GM", "Ford", "UTX", "Merck")

# lost a row due to differencing
fit1 = lm(as.matrix(stocks_diff) ~ FF_data$Mkt.RF)
summary(fit1)

## See the solutions and the two files I generated.
# Depending on the data used, the answer changes.
# For 2004 to 2005:
# We reject the null for GM, Ford, at 0.025 significnace level
# For 2004 to 9/1/2006: We accept the nulls
# Thus, we conclude that GM and Ford are overpriced since they both have negative intercepts.


## Problem 6

cor(residuals(fit1))
res = residuals(fit1)
cor.test(res[,"GM"],res[,"Ford"])
cor.test(res[,"GM"],res[,"UTX"])
cor.test(res[,"GM"],res[,"Merck"])
cor.test(res[,"Ford"],res[,"UTX"])
cor.test(res[,"Ford"],res[,"Merck"])
cor.test(res[,"UTX"],res[,"Merck"])
#For 95% confidence intervals, call cor.test. See solutions

#The correlation of GM And Merck residuals is statistically significant, but may not be practically significant

## Problem 7
attach(FF_data)
sigF = var(Mkt.RF)
bbeta = as.matrix(fit1$coef)
bbeta = bbeta[-1,] # delete intercepts so bbeta has the four slopes
n = dim(stocks_diff)[1]
sigeps = as.matrix((var(as.matrix(res))))
sigeps = diag(as.matrix(sigeps))
sigeps = diag(sigeps,nrow=4)
cov_equities = sigF* bbeta %*% t(bbeta) + sigeps #CAPM model
cov_equities
cov(stocks_diff)

#There is a large discrepancy with the GM/Ford covariance. The rest are similar
#Since these two stocks have a high residual correlation and
#the CAPM assumes that the residual correlation is 0, it is not surprising that
#the CAPM estimated covariance matrix severely underestimates the correlation
#between GM and Ford.


## Problem 8
# Fama French
fit2 = lm(as.matrix(stocks_diff) ~ FF_data$Mkt.RF +
            FF_data$SMB + FF_data$HML)
summary(fit2)

#The CAPM makes the prediction that the slopes for SMB and HML will be zero, because in the CAPM, there is no
# dependence on HML and SMB.
# According to the coefficient output, the p-value for SMB is 0.1, meaning it is very weakly significant, but for HML
# It is statistically significant at the 99% level

## Problem 9
#Problem 9 If the Fama–French model explains all covariances between the
#returns, then the correlation matrix of the residuals should be diagonal. What
#is the estimated correlations matrix? Would you accept the hypothesis that the
#correlations are all zero?
sigF = as.matrix(var(cbind(Mkt.RF,SMB,HML)))
bbeta = as.matrix(fit2$coef)
bbeta = t( bbeta[-1,])# delete intercepts so bbeta has the four slopes
#tranpsose it too

n = dim(stocks_diff)[1]
sigeps = (n-1)/(n-4) * as.matrix((var(as.matrix(fit2$resid))))
sigeps = diag(as.matrix(sigeps))
sigeps = diag(sigeps, nrow=4) #nrow needs to equal the number of stocks used in fit2
cov_FF_factor = bbeta %*% sigF %*% t(bbeta) + sigeps
cor_FF_factor = cov2cor(cov_FF_factor)
cor_FF_sample = cor(fit2$residuals) #sample correlation from the Fama French Fit

#ANSWER TO QUESTION 9
#Read the section on page 536 as a refresher
#summary is this: n = number of returns. p = number of factors
#Typically, n >> p. Computing the SAMPLE correlation matrix is an O(n^2) operation
#Computing the FACTOR correlation matrix is O(n + p^2), and with p << n, this operation is cheaper
#Plus, if you add a new stock, you'd have to compute the entire sample matrix all over
# whereas in the factor case, you just perform another regression.

#Keep in mind the factor cov model has an issue: it assumes zero residual correlation between the variables
#this is what introduces bias and other forms of errors

#When echoing the results, we see the following: GMFord cor is lower in the factor, GM UTX is substantially higher in the factor
# and Merck GM is higher by a little bit. Ford UTX is substantially higher, Ford Merck and Merck UTX are slightly positive and higher

#The Fama French Factor correlation matrix estimations are not all zero. We fail to reject the hypothesis


## PROBLEM 10
#fit1, capm has 1 factor, 
#fit2, fama french, has 3

#You can't take AIC of a multivariate, so we do it one by one and compare
#gm
capm_gm = lm(as.matrix(stocks_diff[, 1]) ~ FF_data$Mkt.RF)
capm_ford = lm(as.matrix(stocks_diff[, 2]) ~ FF_data$Mkt.RF)
capm_utx = lm(as.matrix(stocks_diff[, 3]) ~ FF_data$Mkt.RF)
capm_merck = lm(as.matrix(stocks_diff[, 4]) ~ FF_data$Mkt.RF)

AIC_CAPM = AIC(capm_gm) + AIC(capm_ford) +  AIC(capm_utx) +  AIC(capm_merck)
BIC_CAPM = BIC(capm_gm) + BIC(capm_ford) +  BIC(capm_utx) +  BIC(capm_merck)

FF3_gm = lm(as.matrix(stocks_diff[, 1]) ~ FF_data$Mkt.RF + FF_data$SMB + FF_data$HML)
FF3_ford = lm(as.matrix(stocks_diff[, 2]) ~ FF_data$Mkt.RF + FF_data$SMB + FF_data$HML)
FF3_utx = lm(as.matrix(stocks_diff[, 3]) ~ FF_data$Mkt.RF + FF_data$SMB + FF_data$HML)
FF3_merck = lm(as.matrix(stocks_diff[, 4]) ~ FF_data$Mkt.RF + FF_data$SMB + FF_data$HML)

AIC_FF3 = AIC(FF3_gm) + AIC(FF3_ford) +  AIC(FF3_utx) +  AIC(FF3_merck)
BIC_FF3 = BIC(FF3_gm) + BIC(FF3_ford) +  BIC(FF3_utx) +  BIC(FF3_merck)

#> AIC_CAPM
#[1] 7389
#> BIC_CAPM
##[1] 7440
#> AIC_FF3
#[1] 7369
#> BIC_FF3
#[1] 7453

#ANSWER TO PROBLEM 10
#What we see from the AIC and BIC iterative sum estimation method is that the AICs and BICs are all very similar
#The minimum however is the AIC from the FF3, followed by AIC from CAPM
#this suggests that FF is a better model than CAPM, which makes sense as it explains more information

#The BICs are all high, and smaller for the CAPM. This is probably because the BIC is penalizing the number of parameters
#more heavily since the sample size is large

#PROBLEM 11
cov(fit2$coefficients)

#We can see from the sample covariance matrix of the factors that they enjoy some positive correlation
#but we can't tell how much

cor(fit2$coefficients)
#This more clearly shows us the correlations. GM/F, GM/UTX are heavily correlated

#PROBLEM 12
# Using formulas explained on page 536
#18.11, 18.12, 18.13

Beta = cbind(c(0.5,0.4,-0.1),c(0.6,0.15,0.7))
SigmaEPS = cbind(c(23,0),c(0,37))
SigmaF = cov(FF_data)[1:3,1:3]


SigmaR = t(Beta) %*%SigmaF %*% Beta + SigmaEPS

#Thus, the answers are as follows
#a) Var excess return on stock 1 is SigmaR[1,1] = 23.23
#b) Var excess on stock 2 = SigmaR[2,2] = 37.21
#c) cov between stock 1 and 2 = Sigma[1,2] = SigmaR[2,1] = 0.19

#18.8.3 Statistical Factor Models


dat = read.csv("Stock_FX_Bond.csv")
stocks_ac = dat[ , c(3, 5, 7, 9, 11, 13, 15, 17)]
n = length(stocks_ac[ , 1])
stocks_returns = log(stocks_ac[-1, ] / stocks_ac[-n, ])
fact = factanal(stocks_returns, factors = 2, rotation = "none")
print(fact)
#print(fact, cutoff = 0)

## PROBLEM 13. From the print statement, we see the factor loadings . The variances of the unique risks are
# 0.399   0.423 for GM and F respectively


## PROBLEM 14
#The likelihood ratio test suggests 2 factors are not enough.
#If you manually run the code with factors = 4, you get a p-value of 0.86, suggesting that 4 factors are enough

## PROBLEM 15
loadings = matrix(as.numeric(loadings(fact)), ncol = 2)
unique = as.numeric(fact$unique)
options(digits=2)

##estimated correlation matrix
loadings %*% t(loadings) + diag(unique)
#sample correlation matrix
cor(stocks_returns)

#They are both fairly similar