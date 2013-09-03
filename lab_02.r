bondvalue <- function(c, T, r, par) {
  # Computes bond value (current price)
  # INPUT
  #   c = coupon
  #   T = time to maturity (years)
  #   r = vector of yeald to maturitt (semiannual rates)
  #   par = par value
  bv = c/r + (par - c/r) * (1+r)^(-2*T)
  bv
}

price = 1200
C = 40
T = 30
par = 1000

r = seq(0.02, .05, length = 300)
value = bondvalue(C, T, r, par)
yield2M = spline(value, r, xout=price)

plot(r, value, xlab = 'yield to maturity', ylab = 'price', type = 'l',
     main = 'par = 1000, C = 40, T = 30', lwd = 2)
abline(h = 1200)
abline(v = yield2M)

#### Part 2

library(fEconfin)

plot(mk.maturity[, 1], mk.zero2[5,2:56], type = "l", 
     xlab = "maturity", ylab = "yield")
lines(mk.maturity[, 1], mk.zero2[6, 2:56], lty = 2, type = "l")
lines(mk.maturity[, 1], mk.zero2[7, 2:56], lty = 3, type = "l")
lines(mk.maturity[, 1], mk.zero2[8, 2:56], lty = 4, type = "l")
legend("bottomright", c("1985-12-01", "1986-01-01", "1986-02-01", "1986-03-01"), lty = 1:4)