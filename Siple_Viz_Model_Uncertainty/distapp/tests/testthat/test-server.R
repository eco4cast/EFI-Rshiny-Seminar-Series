# Example test
testServer(server, {
  session$setInputs(island = c("Torgerson","Biscoe"))
  expect_equal(numSelected(),2)
})