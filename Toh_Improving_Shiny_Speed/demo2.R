# Demo 2

# Install package here
remotes::install_github('SEEG-Oxford/ageStand')
library(ageStand)

# Let's say there are 10000 prevalence values to convert
# e.g. 100*100 pixels
r <- runif(10000)

# First way
## Put all 10000 values through the function `convertPrevalence()`
s1 <- convertPrevalence(r, age_min_in = rep(0, 10000),
                        age_max_in = rep(5, 10000))

# Second way
## Because prevalence -> incidence is 1-to-1 and
## monotonically increasing, ask `convertPrevalence()`
## to return value for 0, 0.001, 0.002, ..., 0.999, 1
## Then use `quantile()` to match the values in r to the sequence
## of returned values
pre_calc <- convertPrevalence(seq(0, 1, by = 0.001), 
                              age_min_in = rep(0, 1001),
                              age_max_in = rep(5, 1001))
prev_conv <- quantile(pre_calc, r)

# To show that both ways are approximately equivalent
plot(s1, prev_conv)