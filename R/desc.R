#'@title descAPP for Descriptive statistics and Visualization
#'@description
#'descApp() function opens up a shiny app which will allow you to easily
#'calculate Summary Statistics, Summary Statistics by Group, Box plot,
#'Histogram, Q-Q plot and Shapiro-Wilk's test by uploading CSV file.
#' @details
#' Shiny App for descriptive and exploratory analysis
#' @keywords
#' descrpitive statistics
#' histogram
#' q-q plot
#' box plot
#' summary statistics
#' summary statistics by group
#' @usage
#' descApp()
#' @export
#' @examples
#' \dontrun{
#' library(grapes)
#' grapes::descAPP()
#' }
descApp<- function() {
  appDir <- system.file("desc", package = "grapes")
  if (appDir == "") {
    stop("Could not find directory. Try re-installing `grapes`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
