niter = 1e5

p0 = 1000
less = rep(0, niter)

for (i in 1:niter) {
  p1 = log(p0) + sum(rnorm(5, mean = 0.001, sd = 0.015))
  less[i] = as.numeric(exp(p1) < 990)
}

mean(less)


