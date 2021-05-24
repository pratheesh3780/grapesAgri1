#'@title corrAPP for correlation Analysis
#'@description
#'corrApp() function opens up an interactive shiny app which will
#'allow the user to easily calculate Simple correlation, Correlation Matrix and obtain plots
#'like correlogram and scatterplot by uploading CSV file.
#'@details
#'Shiny App for correlation Analysis
#'@keywords
#'Correlation
#'Correlation Matrix
#'Correlogram
#'Scatter plot
#'@usage
#'corrApp()
#'@export
#'@examples
#'\dontrun{
#'library(grapes)
#'grapes::corrAPP()
#' }
corrApp<- function() {
  appDir <- system.file("Corr", package = "grapes")
  if (appDir == "") {
    stop("Could not find directory. Try re-installing `grapes`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
