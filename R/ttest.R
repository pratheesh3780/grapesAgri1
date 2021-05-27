#'@title test and Paired t-test
#'@description
#'ttApp() function opens up an interactive shiny app which will allow
#'user to easily perform one sample t-test, unpaired two sample t-test
#'unpaired two sample Welch t-test, paired t-test, test for homogeneity of variance (F-test),
#'and obtain plots like boxplot and paired plot by uploading CSV file.
#'@details
#'Shiny App for t-test
#'@keywords
#'one sample t-test
#'unpaired two sample t-test
#'Welch t-test
#'paired t-test
#'Testing homogeneity of variance (F-test),
#'boxplot and paired plot
#'@usage
#'ttApp()
#'@export
#'@examples
#'\dontrun{
#'library(grapes)
#'grapes::ttAPP()
#' }
ttApp<- function() {
  appDir <- system.file("comp_mean", package = "grapes")
  if (appDir == "") {
    stop("Could not find directory. Try re-installing `grapes`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
