context("app-file")
library(shinytest)
test_that("ttApp_testing", {
  skip_on_cran()
  appDir <- system.file("comp_mean", package = "grapesAgri1")
  expect_pass(testApp(appDir, compareImages = FALSE))
})
