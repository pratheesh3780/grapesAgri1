#'@title Correlation Analysis
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
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package \"ggplot2\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("corrplot", quietly = TRUE)) {
    stop("Package \"corrplot\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("Hmisc", quietly = TRUE)) {
    stop("Package \"Hmisc\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("reshape2", quietly = TRUE)) {
    stop("Package \"reshape2\" needed for this function to work. Please install it.",
         call. = FALSE)
  }


  appDir <- system.file("Corr", package = "grapes")
  if (appDir == "") {
    stop("Could not find directory. Try re-installing `grapes`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
