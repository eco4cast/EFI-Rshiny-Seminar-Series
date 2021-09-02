# Demo 3

library(dplyr)
library(readr)
library(data.table)
library(microbenchmark)

# Function to generate fake data frame based on nrow and ncol
gen_fake_df <- function (nrow, ncol, seed) {
  set.seed(seed)
  mat <- matrix(rnorm(nrow*ncol), nrow=nrow, ncol=ncol)
  df <- as.data.frame(mat)
  
  return(df)
}

# Result table
res_read <- data.frame(
  nrow = 10^(1:5),
  base = NA,
  readr = NA,
  data.table = NA
)

# Loop through number of rows, record time used to read
# CSV using different packages
for (i in 1:5) {
  df <- gen_fake_df(10^i, 100, 1314)
  fwrite(df, "tmp.csv")
  res_read$base[i] <- system.time(read.csv("tmp.csv"))[3]
  res_read$readr[i] <- system.time(read_csv("tmp.csv"))[3]
  res_read$data.table[i] <- system.time(fread("tmp.csv"))[3]
}


# Plot the results
plot(log10(res_read$nrow), res_read$base, type = "l",
     xlab = "Number of rows (log10)", ylab = "Time (s)",
     main = "Reading CSV time")
lines(log10(res_read$nrow), res_read$readr, col = "blue")
lines(log10(res_read$nrow), res_read$data.table, col = "red")
legend("topleft", legend = c("base", "readr", "data.table"),
       lty = 1, col = c("black", "blue", "red"))

res_read

# If possible, consider using RDS format
write_rds(df, "tmp.rds")
system.time(readRDS("tmp.rds"))

# Removing tmp files
file.remove("tmp.csv")
file.remove("tmp.rds")

# Filtering
dt <- as.data.table(df) # data.table has different "table" format
microbenchmark(
  df1<-df[df$V1 < 0,], # Base
  df1<-df %>% dplyr::filter(V1 < 0), # dplyr
  dt1<-as.data.table(df)[V1 < 0,], # data.table with conversion
  dt1<-dt[V1 < 0,] # data.table without conversion
)
