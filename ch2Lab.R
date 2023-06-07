##INSTALL the package "rstudioapi" to automate working directories.

##Figure out where this file is located, then set the working directory to this filepath
## This lets us open and close files using local paths
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#Chapter 2 Lab
dat = read.csv("Stock_bond.csv", header = TRUE)
names(dat)
attach(dat)
par(mfrow = c(1, 2))
#AC stands for adjusted closing price
plot(GM_AC, type = "l")
plot(F_AC, type = "b")
n = dim(dat)[1]
GMReturn = GM_AC[-1] / GM_AC[-n] - 1 # These are NET returns
FReturn = F_AC[-1] / F_AC[-n] - 1
par(mfrow = c(1, 1))
plot(GMReturn, FReturn)
#Problem 1: Yes, GM and Ford seem positively correlated. Yes there are outlying
#returns. Yes the GM outliers seem to coincide with Ford outliers


####
# Alternate ways to compute NET returns: (for long positions only)
# R <- priceVec[-1] / priceVec[-length(priceVec)] - 1
# If you have a matrix:
#  R <- priceMat[-1, ] / priceMat[-nrow(priceMat), ] - 1
# For log returns, do log(R + 1) OR
# r <- diff(log(prices))

#Problem2:
GMLReturn = log(1+GMReturn) # Transform a net return to a log return
plot(GMReturn, GMLReturn)
cor(GMReturn, GMLReturn)
#Prob 2 write up: The returns and log returns are highly correlated, as expected.
# The correlation is 0.9995408

#Problem 3:
MSFTReturn = MSFT_AC[-1] / MSFT_AC[-n] - 1 # These are NET returns
MRKReturn = MRK_AC[-1] / MRK_AC[-n] - 1
par(mfrow = c(1, 1))
plot(MSFTReturn, MSFTReturn)
# MRK and MSFT are even more correlated thatn GM and Ford, they are almost in a straight line.
# Same behavior of outliers

#Problem 4
niter = 1e5 #number of iterations we run the simulation
below = rep(0, niter) #set up storage. rep is the repeat function. This is essentially np.zeros(niter)
set.seed(2009) #set a random seed so that the random number generation is identical to the book
for (i in 1:niter)
{
  #Sample a random number from a normal with n=45 observations (45 trading days).
  #The daily log returns have a mean of 0.05 per year, and an annual vol of 0.23 per year
  # The conversions are to get them to daily mean/sd
  r = rnorm(45, mean = 0.05/253,
            sd = 0.23/sqrt(253))
  logPrice = log(1e6) + cumsum(r)
  minlogP = min(logPrice) #minimum price over next 45 days
  below[i] = as.numeric(minlogP < log(950000))
}
mean(below) #this represents the ratio of days that will fall below $950,000 in Fair Mkt Value

#Problems 5, 6, 7, and 8
p5 = rep(0, niter)
p6 = rep(0, niter)
profit = rep(0, niter)
return = rep(0, niter)
ndays = 100
for (i in 1:niter)
{
  r = rnorm(ndays, mean = 0.05/253,
            sd = 0.23/sqrt(253))
  Price = 1e6
  for (j in 1:ndays)
  {
    #P_t = P_t-1*(1+R_t), R_t is the net return
    #P_t = P_t-1*(exp(r_t)), where r_t is the log return
    #We are assuming the *log* returns are normally distributed, so we use the 2nd formula
    Price = Price * exp(r[j])
    if (Price > 1.1e6) {
      p5[i] = 1
      return[i] = ((Price/50e3) - 1) / j #get the daily return on this path
      profit[i] = Price - 1e6
      #p6[i] = 0, this is implicit since it's initialized to zero
      break
    }
    
  }
  
  p6[i] = as.numeric( Price < 1e6 )
  if (return[i] == 0){ #if we didn't set return[i] earlier on a profitable path, set it now
    return[i] = ((Price/50e3) - 1) / ndays
    profit[i] = Price - 1e6
  } 
  
}

Prob_atleast_100k = mean(p5)
prob_loss = mean(p6)
expected_profit = mean(profit)
expected_return = mean(return)

##The answer to Problem 5, probability of at least 100k, is 0.52
##The answer to Problem 6, the prob of a loss, is 37.35%
##The answer to Problem 7, the Expected profit is 20347
##The answer to Problem 8, the expected daily return is 49%
# This can't be right.
# Can modify this to do ROI, total return, etc, take another look


# Problem 9
set.seed(2012)
n = 253
par(mfrow=c(3,3))
for (i in (1:9))
{
  logr = rnorm(n, 0.05 / 253, 0.2 / sqrt(253))
  price = c(120, 120 * exp(cumsum(logr)))
  plot(price, type = "b")
}
#problem 9: From the code, the mean should be 0.05 or 5%, and the std dev should be 0.2, or 20%
#Problem 10: Momentum is real if there is successive positive returns or successive negative returns,
# which it appears the plots do have
#Problem 11: The line of code price = c(120, ...) combines the two arguments into a vector of length 254 (1 + 253)
# with its' first element 120, and it's 2:254 elements 120 * the cumualtive sum
# the cumsum returns a vector

#Problem 12
data = read.csv('MCD_PriceDaily.csv')
head(data)
adjPrice = data[, 7]
mcd_nret = adjPrice[-1] / adjPrice[-length(adjPrice)] - 1 # These are NET returns
mcd_lret = log(1+mcd_nret) # Transform a net return to a log return
par(mfrow=c(1,1))
plot(mcd_nret, mcd_lret)
#Yes, it appears the returns are approx equal. This is reasonable since log(1+x) is about = to x when x is small

#Problem 13
options(digits = 4)
r_mean = mean(mcd_nret)
r_sd = sd(mcd_nret)
lr_mean = mean(mcd_lret)
lr_sd = sd(mcd_lret)

#Problem 14
returns = mcd_nret
log_returns = mcd_lret
t_test = t.test(returns, log_returns, paired = TRUE)

#Problem 15
# Yes, they are interchangeable for small enough values, but for the sake of precision should not be done unless needed

#Problem 16, Yes see my lab


