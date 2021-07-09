context("app-file")
library(shinytest)
test_that("layoutApp_testing", {
  skip_on_cran()
  appDir <- system.file("layout", package = "grapesAgri1")
  expect_pass(testApp(appDir, compareImages = FALSE))
})
