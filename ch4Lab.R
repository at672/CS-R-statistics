setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list=ls())
data(EuStockMarkets)
mode(EuStockMarkets)
class(EuStockMarkets)
plot(EuStockMarkets)
pdf("EuStocks.pdf", width = 6, height = 5)
plot(EuStockMarkets)
graphics.off()

#Problem 1:
#The time series plots are mostly flat until about 1997 at which point they start to increase and have more volatility
#The series do not look stationary. Stationarity implies a constant mean, but the mean rises in the following years
#Defn of weak stationarity( page 308): Y1, Y2, ... is weakly stationary if 
# E(Y_t) = mu ( a finite constant for all t)
# Var(Y_t) = sigma^2 ( a positive finite constant) for all t
# Cov(Yt, Ys) = gamma(|t-s|) for all t and s, and for some function gamma(h)
# weak stationarity is sometimes referred to as covariance stationarity
# The fluctuations do not appear to be of constant size
# The volatility seems to cluster around specific dates, for all four indicies

#Problem 2:
logR = diff(log(EuStockMarkets))
plot(logR)

#The log returns appear to be stationary (constant mean) and constant variance, although there are a few specific
#date ranges that have some clustering
#Fluctuations do appear to be of constant size

#Problem 3:
plot(as.data.frame(logR)) #plots scatters of one index against the other
par(mfrow=c(2, 2)) #formats 2 row x 2 column
for(i in colnames(logR))
{
  qqnorm(logR[ ,i], datax = T, main = i) # Create a quantile plot for each index with sample quantiles on the X axis
  qqline(logR[ ,i], datax = T) #add a normality line (shows what a normal distribution would be)
  print(shapiro.test(logR[ ,i])) #perform the shapiro wilk test on each index
}

#Problem 3 answer: All four plots show that the 4 indicies are non-normal. Specifically, they are heavy tailed
#distributions.


n=dim(logR)[1]
q_grid = (1:n) / (n + 1)
df_grid = c(1, 4, 6, 10, 20, 30)
index.names = dimnames(logR)[[2]]
for(i in 1:4)
  {
    # dev.new() #only run this if not in RStudio
    par(mfrow = c(3, 2))
    for(df in df_grid)
      {
      #instead of a qq-norm plot, do a qq-t plot
      #data is still on x axis
        qqplot(logR[,i], qt(q_grid,df),
                  main = paste(index.names[i], ", df = ", df) )
        abline(lm(qt(c(0.25, 0.75), df = df) ~
                       quantile(logR[,i], c(0.25, 0.75))))
        }
}

#Problem 4: The line q.grid  = ... Creates a vector of points spaced from 1/1860 to 1859/1860
# The line qt is the quantile function from the student t distribution
# The line paste concatenates strings, in this case the name of the index with degrees of freedom

#Problem 5: For the DAX index, it appears that DF 4 is the best fit. DF 1 is too small
# as it results in a light-tailed distribution, and df6 and above is pretty noticeably heavy tailed


#Problem 6 set up

#Formula: standard_dev = lambda * sqrt( df / (df-2)), lambda is the scale parameter
library("fGarch")
x=seq(-0.1, 0.1, by = 0.001)
par(mfrow= c(1 , 1))
df = 2.001
mad_t = mad(logR[ , 1],
              constant = sqrt(df / (df - 2)) / qt(0.75, df))
plot(density(logR[ , 1]), lwd = 2, ylim = c(0, 60))
lines(x, dstd(x, mean = mean(logR[,1]), sd = mad_t, nu = df),
        lty = 5, lwd = 2, col = "red")
lines(x, dnorm(x, mean = mean(logR[ ,1]), sd = sd(logR[ ,1])),
         lty = 3, lwd = 4, col = "blue")
legend("topleft", c("KDE", paste("t: df = ",df), "normal"),
          lwd = c(2, 2, 4), lty = c(1, 5, 3),
          col = c("black", "red", "blue"))

#Right tail
plot(density(logR[ , 1]), lwd = 2, ylim = c(0, 6), xlim = c(0.025, 0.1))
lines(x, dstd(x, mean = mean(logR[,1]), sd = mad_t, nu = df),
      lty = 5, lwd = 2, col = "red", xlim = c(0.025, 0.1) )
lines(x, dnorm(x, mean = mean(logR[ ,1]), sd = sd(logR[ ,1])),
      lty = 3, lwd = 4, col = "blue", xlim = c(0.025, 0.1))
legend("topleft", c("KDE", paste("t: df = ",df), "normal"),
       lwd = c(2, 2, 4), lty = c(1, 5, 3),
       col = c("black", "red", "blue"))


#Left tail
plot(density(logR[ , 1]), lwd = 2, ylim = c(0, 6), xlim = c(-0.1, -0.025))
lines(x, dstd(x, mean = mean(logR[,1]), sd = mad_t, nu = df),
      lty = 5, lwd = 2, col = "red", xlim = c(-0.1, -0.025) )
lines(x, dnorm(x, mean = mean(logR[ ,1]), sd = sd(logR[ ,1])),
      lty = 3, lwd = 4, col = "blue", xlim = c(-0.1, -0.025))
legend("topleft", c("KDE", paste("t: df = ",df), "normal"),
       lwd = c(2, 2, 4), lty = c(1, 5, 3),
       col = c("black", "red", "blue"))

#I varied the df with 2.001, 2.1, 4, 5, and 10. You'll never have a perfect fit
# Lower DFs made the hump agree as closely as possible
#With the default value of adjust (the bandwidth), it appears that there may be too high variance because of the small
#successive humps in the tails

#Problem 6: The normal model is not a good fit, that's pretty obvious
# The t distribution is is a slightly better fit. Although the tails are too heavy at df = 2.001

#Problem 7
# From calling ?density, the default bandwidth is adjust*bw. 
# There's a long paper about bandwidth's here:
# https://cran.r-project.org/web/packages/kedd/vignettes/kedd.pdf
# The default bandwidth appears to be determined from some function. In this case
# It's about 0.001645

# The default kernel is Gaussian i.e. standard normal pdf

# Problem 8: Skipped. It's very similar to earlier problem

# Problem 9

data = read.csv('MCD_PriceDaily.csv')
head(data)
adjPrice = data[ , 7]
plot(adjPrice, type = "l", lwd = 2)

#Because there is no tendency to mean-revert, it is non-stationary

#Problem 10
n = length(adjPrice)
LogRet = log(adjPrice[-1]/adjPrice[-n])
date = as.Date(data[,1],"%m/%d/%Y")
#pdf("mcd02.pdf",width = 6, height = 5)
plot(date[-1], LogRet, ylab = "MCD log return",type="l",lwd=2)
graphics.off()

#The log returns do appear to be stationary

#Problem 11
par(mfrow=c(1,2))
hist(LogRet, 80, freq = FALSE)
qqnorm(LogRet,datax=TRUE)
qqline(LogRet,datax=TRUE,col="red",lwd=2)

#The histogram doesn't show much of a skew
#The qqnorm shows that the data is heavy tailed
# The log returns are not Gaussian
# Generally it is reasonably symmetric

