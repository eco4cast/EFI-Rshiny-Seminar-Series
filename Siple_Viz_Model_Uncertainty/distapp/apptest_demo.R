
# Demo 2: Writing tests for Shiny -----------------------------------------
# This code assumes you have already created a Shiny app (we have!) but to make a Shiny app folder structure, you can start a new Shiny app with all the right folders and examples by using:
# shinyAppTemplate("myapp")
# If you select 1 from the list of options that comes up, ou will get a full file structure with templates included.


library(testthat)
source("distapp/app.R")


# Example of a testServer() function --------------------------------------
testServer(server, {
  # Give input$island a value.
  session$setInputs(island = "Torgersen")
  
  cat("Now that the island is set to Torgersen, numSelected is: ", numSelected(), "\n")
  
  # Now update input$island to a new value
  session$setInputs(island = c("Torgerson","Biscoe"))
  
  # Check whether it works
  cat("After adding another island to the inputs, numSelected is: ", numSelected(), "\n")
})


# Running tests using testthat -------------------------------------------
# Look for the next test in tests/testthat/test-server.R
#
#shiny::runTests("distapp")
#runTests() looks in the tests/ directory and sources the .R files therein
