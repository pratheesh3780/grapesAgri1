context("app-file")
library(shinytest)
test_that("corrApp_testing", {
  skip_on_cran()
  appDir <- system.file("Corr", package = "grapesAgri1")
  expect_pass(testApp(appDir, compareImages = FALSE))
})
