setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list=ls())
library(urca)
midcapD.ts = read.csv("midcapD.ts.csv",header=T)
x = midcapD.ts[,2:11]
prices= exp(apply(x,2,cumsum))
options(digits=3)
summary(ca.jo(prices))
##PROBLEM 1
# How many cointegration vectors were found?

cajo.output = ca.jo(prices)
#cajo.output@V is the Beta, @W is the alpha
#urca stands for unit root and cointegration analysis
#Beta is the cointegration vector, alpha is the loading matrix or adjustment matrix
# alpha specifies the speed of mean reversion
# view function on Page 457 of the book
# The cointegration vectors are the columns of the matrix labeled "Eigenvectors, normalized to first column"

# Thus, we conclude, a total of 10 cointegration vectors were found

# PROBLEM 2
library(urca)
mk.maturity = read.csv("mk.zero2.csv", header=T)
summary(ca.jo(mk.maturity[,2:11]))

## PROBLEM 2: 
# It's difficult to tell what maturities are being used by reading the CSV. The CSV doesn't say.
# However, if we open the csv file mk.maturity, it is a column telling us what the maturities are
# Thus, we conclude that columns 2 to 11 (1 indexed) from the excel csv correspond to the
# 1) 0.0833 year (1 month)
# 2) 0.167 year (2 month)
# 3) 0.25 year ( 3 month)
# 4) 0.333 year (4 month)
# 5) 5 month,
# 6) 6 month, 7) 7 month, 8) 8 month, 9) 9 month, 10, 10 month, 11, 11 month, and 12, 12 month/1 year
# So they are short maturities

## PROBLEM 3
# From the output, we see that are 10 eigenvectors, or 10 cointegration vectors printed.
# For t-test, we reject if the test value > pct, where pct is the percent cutoff
# So, for r <=9, it's test < 1pct, r<=8, test < 1 pct ... but at r<=3 its test > 1 pct
# So we reject r<=3, meaning we accept that r <=4. So we conclude r = 4.
# Thus there are 4 cointegration vectors.

# We can verify this through the following code:
yields.cajo = ca.jo(mk.maturity[,2:11])
Z = as.matrix(mk.maturity[,2:11]) %*% yields.cajo@V[,1]
par(mfrow=c(1,2))
ts.plot(Z)
acf(Z)

#These plots do not show non-stationary behavior

## PROBLEM 4

CokePepsi = read.table("CokePepsi.csv", header=T)
ts.plot(CokePepsi)

#Problem 4: Yes, they appear cointegrated. I can visualize a linear combination that is stationary

## Problem 5 CODE:
ts.plot(CokePepsi[,2] - CokePepsi[,1])
#Yes, the difference appears stationary.
par(mfrow=c(1,1))
acf(CokePepsi[,2] - CokePepsi[,1])
#Plotting the ACF however indicates its nonstationary.

##Problem 6 code:
library(urca)
summary(ca.jo(CokePepsi))

#Problem 6, from the output, we see at r<= 1, the test value is less than the 1pct value, so we fail to reject at that value
# for r = 0, we see the test value is also less than the 1pct value. So we fail to reject again at 1% confidence
# Thus, we fail to reject the null hypothesis. We conclude r = 0, that is there are 0 cointegrating vectors
# We conclude coke and Pepsi are not cointegrated.

Stock_FX_Bond = read.csv("Stock_FX_Bond.csv", header=T)
adjClose = Stock_FX_Bond[,seq(from=3, to=21, by=2)]
ts.plot(adjClose)
summary(ca.jo(adjClose))
cajo_out = ca.jo(adjClose)
rankMatrix(cajo_out@V)
# PROBLEM 7: We see that the test value is greater than the 1pct value at r<=1, but not r = 0
# We conclude the series are cointegrated, with r = 1, meaning that there is only one cointegration vector
# r is the number of linearly independent cointegration vectors. Thus, the rank of the matrix is 1.
# I don't know why rankMatrix is returning 10 here.

summary(ca.jo(adjClose, K=8))

# PROBLEM 8: When running with lag K = 8, we see the only test value > 1pct is at r = 0.
# The cajo test is as follows: Ho : r <= r0, H1: r > r0. r0 = 0, 1, 2, ... , d-1, where d is the number of vectors/columns
# So, we choose r0 = 0, and we check the test value against the 1pct value. test > 1 pct at 1 pct level, so we reject the null
# we conclude that r > 0
# But, for r<=1, we apply the same logic and find ourselves rejecting at 10% and 5% levels, but accepting at 1 pct level
# so we conclude r < = 1, r > 0 from these two values
# So we conclude that r = 1 at the 1 pct level.
# We get the same number of cointegration vectors.

##15.5.4 Simulation

niter = 10000 #number of iterations we run the simulation
st_dev_eps = 5000
mu = 1.030e6
phi = 0.99
P0 = 1e6
Profit = rep(0, niter) #set up storage. rep is the repeat function. This is essentially np.zeros(niter)
ndays = 45
set.seed(2009) #set a random seed so that the random number generation is identical to the book
eps = rep(0, niter)
Loss = rep(0, niter)
WaitTime = rep(0, niter)
for (i in 1:niter)
{
  P = rep(0, ndays)
  P[1] = P0
  for (j in 2:ndays)
  {
    eps[j] = rnorm(n=1, mean = 0, sd = st_dev_eps)
    P[j] = mu + phi*(P[j-1]-mu) + eps[j]
    
    if (P[j] >= 1.02e6)
    {
      Profit[i] = P[j] - P[1]
      Loss[i] = FALSE
      WaitTime[i] = j
      break
    }
    if (P[j] < 0.095e6){
      Profit[i] = P[j] - P[1]
      Loss[i] = TRUE
      WaitTime[i] = j
      break
    }
      
  }
  if (Profit[i] == 0){ #if we didn't set Profit[i] earlier on a profitable path, set it now
    Profit[i] = P[ndays] - P[1]
    WaitTime[i] = j
    if (Profit[i] < 0){
      Loss[i] = TRUE
    }
    else{
      Loss[i] = FALSE
    }
  }
}

mean(Profit)
var(Profit)
mean(WaitTime)
YearlyReturn = 253/45 * mean(Profit)/P0

#Problem 9: The expected profit is mean(Profit) = 9562
# Problem 10: The probability of liquidiating for a loss is mean(Loss) 0.2708
# Problem 11: The expected waiting time is 29.3324 days
# Problem 12: The expected yearly return is 5.37%