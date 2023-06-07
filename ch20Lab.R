setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list=ls())

#20.15.1 Fitting a t-Distribution by MCMC
library(rjags)
data(CRSPmon, package = "Ecdat")
ibm = CRSPmon[ , 2]
r = ibm
N = length(r)
ibm_data = list(r = r, N = N)
inits = function(){list(mu = rnorm(1, 0, 0.3),
                        tau = runif(1, 1, 10), k = runif(1, 1, 30))}
univ_t <- jags.model("univt.bug", data = ibm_data,
                     inits = inits, n.chains = 3, n.adapt = 1000, quiet = FALSE)
nthin = 2
univ_t.coda = coda.samples(univ_t, c("mu", "tau", "k",
                                     "sigma"), n.iter = 500 * nthin, thin = nthin)
summary(univ_t.coda)
effectiveSize(univ_t.coda)
gelman.diag(univ_t.coda)

##Problem 1
# (a) Which parameter mixes best according to Neff in the output?
# Sigma mixes best since it has the larger N_eff
# (b) Which parameter mixes worst according to Neff in the output?
# tau mixes worst
# (c) Give a 95% posterior interval for the degrees-of-freedom parameter.
#  The 95% posterior interval for k (equal tail weights) is taken from the 2.5% and 97.5% quantile outputs
# Thus we choose 7.318 and 29.425

## Problem 2
gelman.plot(univ_t.coda)
par(mfrow = c(2, 2))
traceplot(univ_t.coda)
par(mfrow = c(2, 2))
autocorr.plot(univ_t.coda, auto.layout = FALSE)

# 2a) From my plots, it seems tau mixes worst and sigma mixes best

k1 = univ_t.coda[[1]][,1]
k2 = univ_t.coda[[2]][,1]
k3 = univ_t.coda[[3]][,1]
k = c(k1,k2,k3)
std_k = (k-mean(k)) / sqrt(mean((k-mean(k))^2))
options(digits=4)
mean(std_k^3)
mean(std_k^4)

# 2b) The skew and kurtosis are the standardized k's  raised to third and fourth powers respectively, as shown above

library(lattice)
densityplot(univ_t.coda)

#PROBLEM 3: Tau is the most skewed

