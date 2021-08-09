context("app-file")
library(shinytest)
test_that("descApp_testing", {
  skip_on_cran()
  appDir <- system.file("desc", package = "grapesAgri1")
  expect_pass(testApp(appDir, compareImages = FALSE))
})
