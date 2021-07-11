context("app-file")
library(shinytest)
test_that("crdApp_testing", {
  skip_on_cran()
  appDir <- system.file("crd", package = "grapesAgri1")
  expect_pass(testApp(appDir, compareImages = FALSE))
})
