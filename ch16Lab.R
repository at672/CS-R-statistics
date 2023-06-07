setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list=ls())

# 16.10.1 Efficient Equity Portfolios PAGE 489

dat = read.csv("Stock_Bond.csv", header = T)
prices = cbind(dat$GM_AC, dat$F_AC, dat$CAT_AC, dat$UTX_AC,
               dat$MRK_AC, dat$IBM_AC)
n = dim(prices)[1]
returns = 100 * (prices[2:n, ] / prices[1:(n-1), ] - 1)
pairs(returns)
mean_vect = colMeans(returns)
cov_mat = cov(returns)
sd_vect = sqrt(diag(cov_mat))

# PROBLEM 1 CODE
M = length(mean_vect)
library(quadprog)
Amat = cbind(rep(1,M),mean_vect,diag(1,nrow=M),-diag(1,nrow=M))
muP = seq(min(mean_vect)+.02,max(mean_vect)-.02,length=10)
muP = seq(.05,0.08,length=300)
sdP = muP
weights = matrix(0,nrow=300,ncol=M)
for (i in 1:length(muP))
{
  result =
    solve.QP(Dmat=cov_mat,dvec=rep(0,M), Amat=Amat,
             c(1,muP[i],rep(-.1,M),rep(-.5,M)), meq=2)
  sdP[i] = sqrt(2*result$value)
  weights[i,] = result$solution
}
plot(sdP,muP,type="l",xlim=c(0,2.5),ylim=c(0,.1))
mufree = 3/365
points(0,mufree,cex=3,col="blue",pch="*")
sharpe =( muP-mufree)/sdP
ind = (sharpe == max(sharpe)) # locates the tangency portfolio
weights[ind,] # weights of the tangency portfolio
lines(c(0,sdP[ind]),c(mufree,muP[ind]),col="red",lwd=3)
points(sdP[ind],muP[ind],col="blue",cex=3,pch="*")
ind2 = (sdP == min(sdP)) #locates the minimum variance portfolio
points(sdP[ind2],muP[ind2],col="green",cex=3,pch="*")
ind3 = (muP > muP[ind2]) #Locates the set of points with the highest returns associated with given risk level
lines(sdP[ind3],muP[ind3],type="l",xlim=c(0,.25),
      ylim=c(0,.3),col="cyan",lwd=3)
text(sd_vect[1],mean_vect[1],"GM")
text(sd_vect[2],mean_vect[2],"F")
text(sd_vect[3],mean_vect[3],"UTX")
text(sd_vect[4],mean_vect[4],"CAT")
text(sd_vect[5],mean_vect[5],"MRK")
text(sd_vect[6],mean_vect[6],"IBM")
legend("topleft",c("efficient frontier","efficient portfolios",
                   "tangency portfolio","min var portfolio"),
       lty=c(1,1,NA,NA),
       lwd=c(3,3,1,1),
       pch=c("","","*","*"),
       col=c("cyan","red","blue","green"),
       pt.cex=c(1,1,3,3)
)

## PROBLEM 2: # check solutions
# basically it's asking to compute an optimization with additional constraints
# E(daily return) = 0.07%
# -0.1 <= w_j <= 0.5
# let omega be the weight on the risk free asset and mu_f be the risk free return
# let mu_T be the expected return of the tangency portfolio
# We know mu_port = omega*mu_f + (1-omega)*mu_T, mu_port = 0.07, which is daily
# solve the equation for omega
options(digits=3)
omega = (.07 - muP[ind]) / (3/265 - muP[ind])
omega
1-omega
(1-omega)*weights[ind]
omega + sum((1-omega)*weights[ind])

#omega is the weight for the risk free, and the weights for the six stocks are
#(1-omega)*weights[ind]

# PROBLEM 3
# Yes, check spreadsheet


# 16.10.2 Efficient Portfolios with Apple, Exxon-Mobil, Target,
# and McDonald’s Stock

dat = read.csv("FourStocks_Daily2013.csv", header = TRUE)
head(dat)
prices = dat[,-1]
n = dim(prices)[1]
returns = 100*(prices[-1,] / prices[-n,] - 1)
# same set up as in problem 1:
pairs(returns)
mean_vect = colMeans(returns)
cov_mat = cov(returns)
sd_vect = sqrt(diag(cov_mat))

M = length(mean_vect)
library(quadprog)
Amat = cbind(rep(1,M),mean_vect,diag(1,nrow=M),-diag(1,nrow=M))
#muP = seq(min(mean_vect)+.02,max(mean_vect)-.02,length=10)
muP = seq(.045,0.06,length=300) #From problem specification
sdP = muP
weights = matrix(0,nrow=300,ncol=M)
for (i in 1:length(muP))
{
  result =
    solve.QP(Dmat=cov_mat,dvec=rep(0,M), Amat=Amat,
             c(1,muP[i],rep(-.5,M),rep(-.5,M)), meq=2)
  sdP[i] = sqrt(2*result$value)
  weights[i,] = result$solution
}
plot(sdP,muP,type="l",xlim=c(0,2.5),ylim=c(0,.1))
mufree = 1.3/365
points(0,mufree,cex=3,col="blue",pch="*")
sharpe =(muP - mufree)/sdP
ind = (sharpe == max(sharpe)) # locates the tangency portfolio
weights[ind,] # weights of the tangency portfolio
lines(c(0,sdP[ind]),c(mufree,muP[ind]),col="red",lwd=3)
points(sdP[ind],muP[ind],col="blue",cex=3,pch="*")
ind2 = (sdP == min(sdP)) #locates the minimum variance portfolio
points(sdP[ind2],muP[ind2],col="green",cex=3,pch="*")
ind3 = (muP > muP[ind2]) #Locates the set of points with the highest returns associated with given risk level
lines(sdP[ind3],muP[ind3],type="l",xlim=c(0,.25),
      ylim=c(0,.3),col="cyan",lwd=3)
text(sd_vect[1],mean_vect[1],"AAPL")
text(sd_vect[2],mean_vect[2],"XOM")
text(sd_vect[3],mean_vect[3],"TGT")
text(sd_vect[4],mean_vect[4],"MCD")
legend("topleft",c("efficient frontier","efficient portfolios",
                   "tangency portfolio","min var portfolio"),
       lty=c(1,1,NA,NA),
       lwd=c(3,3,1,1),
       pch=c("","","*","*"),
       col=c("cyan","red","blue","green"),
       pt.cex=c(1,1,3,3)
)

## Problem 5
# Skipped for now


# 16.10.3 Finding the Set of Possible Expected Returns
dat = read.csv("Stock_Bond.csv", header = T)
prices = cbind(dat$GM_AC, dat$F_AC, dat$CAT_AC, dat$UTX_AC,
                 dat$MRK_AC, dat$IBM_AC)
n = dim(prices)[1]
returns = 100 * (prices[2:n, ] / prices[1:(n-1), ] - 1)
mean_vect = colMeans(returns)
M = length(mean_vect)
B1 = 0.3
B2 = 0.1
library(linprog)
AmatLP1 = cbind(diag(1, nrow = M), matrix(0, nrow = M, ncol = M))
AmatLP2 = cbind(matrix(0, nrow = M, ncol = M), diag(1, nrow = M))
AmatLP3 = c(rep(1, M), rep(-1, M))
AmatLP = rbind(AmatLP1, AmatLP2, AmatLP3)
bvecLP = c(rep(B1, M), rep(B2, M), 1)
cLP = c(mean_vect, -mean_vect)
const.dir = c(rep("<=", 2 * M), "=")
resultLP_min = solveLP(cvec = cLP, bvec = bvecLP, Amat = AmatLP,
                          lpSolve=T, const.dir = const.dir, maximum = FALSE)
resultLP_max = solveLP(cvec = cLP, bvec = bvecLP,
                          Amat = AmatLP, lpSolve = TRUE, maximum = TRUE)

#Problem 6
# Q: What is the set of feasible expected portfolio returns when −0.1 ≤
# wi ≤ 0.3 for all i?
# A: Since we solved for both the min expected return and the max expected return,
# The set lies in the interval [resultLP_min$opt, resultLP_max$opt]
# For the allocation vectors, call resultLP_min$solution
# The first M values (M=6) correspond to w_i <= B1, and the next M correspond to -B2 <= w_i
# B2 is a positive number, but because of how we set up the problem (see the construction of AmatLP)
# The w_i's in this half are negative
# So, for example, for the min solution, go long 1,2,5,6 at 0.3 each (1.2)
# and go short 9 and 10 at -0.1 each (totals to 1.0 weight)

#Problem 7
B1 = 0.15
B2 = 0.15
library(linprog)
AmatLP1 = cbind(diag(1, nrow = M), matrix(0, nrow = M, ncol = M))
AmatLP2 = cbind(matrix(0, nrow = M, ncol = M), diag(1, nrow = M))
AmatLP3 = c(rep(1, M), rep(-1, M))
AmatLP = rbind(AmatLP1, AmatLP2, AmatLP3)
bvecLP = c(rep(B1, M), rep(B2, M), 1)
cLP = c(mean_vect, -mean_vect)
const.dir = c(rep("<=", 2 * M), "=")
resultLP2_min = solveLP(cvec = cLP, bvec = bvecLP, Amat = AmatLP,
                       lpSolve=T, const.dir = const.dir, maximum = FALSE)
resultLP2_max = solveLP(cvec = cLP, bvec = bvecLP,
                       Amat = AmatLP, lpSolve = TRUE, maximum = TRUE)

#When doing this, we find a maximum return, but no minimum return. Think about what B1 and B2 are. They are the limits
# on both the long and the short weights. The limits on the max return are 0.15*6 = 0.9, so they aren't even adding to 1.
# For B2, the optimization fails outright.
# If you change to 0.14, 0.16 we see that the slacks are still present. The 