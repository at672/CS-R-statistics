#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
bondvalue = function(c, T, r, par)
{
  # Computes bv = bond values (current prices) corresponding
  # to all values of yield to maturity in the
  # input vector r
  #
  # INPUT
  # c = coupon payment (semiannual)
  # T = time to maturity (in years)
  # r = vector of yields to maturity (semiannual rates)
  # par = par value
  #
  bv = c / r + (par - c / r) * (1 + r)^(-2 * T)
  bv
}

price = 1200 # current price of the bond
C = 40 # coupon payment
T= 30 # time to maturity
par = 1000 # par value of the bond
r = seq(0.02, 0.05, length = 300) #generate a sequence of linearly spaced values between the endpoints
value = bondvalue(C, T, r, par)
yield2M = spline(value, r, xout = price) # spline interpolation
plot(r, value, xlab = 'yield to maturity', ylab = 'price of bond',
     type = "l", main = "par = 1000, coupon payment = 40, T = 30", lwd = 2)
abline(h = 1200)
abline(v = yield2M)

#Problem 1 The spline interpretation returns a yield of 0.0324 for a price of 1200
# This point is on the Price Yield Curve, so yes the spline agrees with the curve

#Problem 2
uniroot(function(r) r^2 - .5, c(0.7, 0.8))

#The function uniroot is a one dimensional root finder that takes in a function and searches between
#the boundaries (0.7, 0.8) for a root with respect to the first argument of the function
# So in this example, the function is f(x) = x^2 - 0.5, written as r^2 - .5 in the code
# Uniroot searches for a zero in the interval 0.7,0.8
# the root of this function is sqrt(2)/2

#Problem 3
T = 30
C = 40
par = 1000
price = 1200
options(digits = 3)
uniroot(function(r) bondvalue(C,T,r,par) - price, c(0.001,.1))
#From the output, the root is 0.0324. This is the yield to maturity for the specified bond

#Problem 4
T = 8
C =35
par = 10000
price = 9800
options(digits = 3)
uniroot(function(r) bondvalue(C,T,r,par) - price, c(0.001,.1))
#The YTM is 0.0048


#Problem 5
T = 20
C = 35
par = 1000
price = 1050
options(digits = 3)
uniroot(function(r) bondvalue(C,T,r,par) - price, c(0.001,.1))
#The YTM is 0.0327

#Problem 6
T = 5
par = 1000
price = 950.10
r = 0.035
options(digits = 3)
uniroot(function(C) bondvalue(C,T,r,par) - price, c(0, 200))
#The coupon payment is $29

#Problem 7
mk.maturity = read.csv("mk.maturity.csv", header = T)
mk.zero2 = read.csv("mk.zero2.csv", header = T)
plot(mk.maturity[,1], mk.zero2[5,2:56], type = "l",
     xlab = "maturity", ylab = "yield", xlim = c(0, 3))
lines(mk.maturity[,1], mk.zero2[6,2:56], lty = 2, type = "l")
lines(mk.maturity[,1], mk.zero2[7,2:56], lty = 3, type = "l")
lines(mk.maturity[,1], mk.zero2[8,2:56], lty = 4, type = "l")
legend("bottomright", c("1985-12-01", "1986-01-01",
                        "1986-02-01", "1986-03-01"), lty = 1:4)
#From dec 1 1985 to mar 1 1986, the yield curve falls and flattens in the 0 to 3 year window
#When looking at the neitre 30 year picture, what we see is that the yield curve still falls and flattens
#with some inversions taking place on the tail ends (past 20 year mark)

#Problem 8
#It involves changing the indexes for mk.zero2 and mk.maturity when you read it, and relabeling it, skipping
#


#Problem 9 set up
#See textbook for explanation of code
#This line estimates integrated forward rate
intForward = mk.maturity[, 1] * mk.zero2[6, 2:56]
xout = seq(0, 20, length = 200) #Creates a grid
z1 = spline(mk.maturity[ ,1], intForward, xout = xout) #Interpolates the int forward onto the grid
forward = diff(z1$y) / diff(z1$x) #numerical differentiation to estimate fwd rate
T_grid = (xout[-1] + xout[-200]) / 2 #approximate the forward rate on the grid
plot(T_grid, forward, type = "l", lwd = 2, ylim = c(0.06, 0.11))

#Problem 9 was done in lab_2, see code there. It's essentially the same set of lines done 4 times