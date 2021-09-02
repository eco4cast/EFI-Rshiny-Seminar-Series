# Demo 1
library(dplyr)
library(microbenchmark)

# Generating random time series
set.seed(1314)
ts1 <- arima.sim(list(ar=0.3), n = 1000, sd = 1)
ts2 <- ts1 * 100 + 1:1000
ts2 <- ifelse(ts2 < 0, 0, ts2)
plot(ts2)

# For loop calculating MA
calc_ma7_forloop <- function (tss) {
  ma7 <- rep(NA, length(tss))
  for (i in 4:(length(tss)-3)) {
    ini <- i-3
    fin <- i+3
    ma7[i] <- mean(tss[ini:fin])
  }
  
  return(ma7)
}

# Use microbenchmark to test running time
microbenchmark(calc_ma7_forloop(ts2),
               stats::filter(ts2, filter = rep(1/7, 7), sides = 2))

# Some visualizations
ma7 <- stats::filter(ts2, filter = rep(1/7, 7), sides = 2)
plot(ts2)
lines(ma7, col = "red", lwd = 2)
