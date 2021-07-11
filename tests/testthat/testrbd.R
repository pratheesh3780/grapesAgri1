context("app-file")
library(shinytest)
test_that("rbdApp_testing", {
  skip_on_cran()
  appDir <- system.file("RBD", package = "grapesAgri1")
  expect_pass(testApp(appDir, compareImages = FALSE))
})
