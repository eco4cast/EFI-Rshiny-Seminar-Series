# This is the runner for all the code tests.
library(testthat)
test_dir(
  "./testthat",
  env = shiny::loadSupport(), 
  # This sources all the files in the R/ subdir in a new environment
  reporter = c("progress", "fail")
)