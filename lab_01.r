niter <- 1e3

nLoss <- 0
nProfit <- 0
nTime <- 0

pnl <- rep(0, niter)
rate <- rep(0, niter)

set.seed(2009)

for(i in 1:niter) {
  r <- rnorm(100, mean = .05/253, sd = 0.23/sqrt(253))
  logPrice <- log(1e6) + cumsum(r)
  
  profit <- as.numeric(logPrice > log(1100000))
  loss <- as.numeric(logPrice < log(950000))
  
  if (sum(profit) > 0) {
    nProfit = nProfit + 1
    profitPos = which.max(profit)
    pnl[i] = exp(logPrice[profitPos])
    rate[i] = (((pnl[i] - 950000)/50000) - 1) / profitPos
  } else if (sum(loss) > 0) {
    nLoss = nLoss + 1
    lossPos = which.max(loss)
    pnl[i] <- exp(logPrice[lossPos])
    rate[i] = (((pnl[i] - 950000)/50000) - 1) / lossPos
  } else {
    nTime = nTime + 1
    pnl[i] = exp(logPrice[100])
    rate[i] = (((pnl[i] - 950000)/50000) - 1) / 100
  }  
}

pLoss = nLoss / niter
pProfit = nProfit / niter
pTime = nTime / niter
meanPnl = mean(pnl)
meanRate = mean(rate)