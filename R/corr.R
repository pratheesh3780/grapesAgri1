#'@title Correlation Analysis
#'@description
#'corrApp() function opens up an interactive shiny app which will
#'allow the user to easily calculate Simple correlation, Correlation Matrix and obtain plots
#'like correlogram and scatterplot by uploading CSV file.
#'@details
#'This app uses \code{cor.test} to calculate correlation. Correlation matrix
#'is calculated using \code{rcorr} function in \code{Hmisc} package. Correlogram
#'is obtained using \code{corrplot} function in \code{corrplot} package
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
#'
#'
#'@references
#'
#'
#'
#'\insertRef{corrplot2021}{grapes}
#'
#'\insertRef{Hmisc_2021}{grapes}
#'
#'\insertRef{reshape2}{grapes}
#'
#'\insertRef{R_2021}{grapes}
#'
#'\insertRef{shiny_2021}{grapes}
#'
#'\insertRef{sw_2021}{grapes}
#'
#'\insertRef{ggplot_2016}{grapes}
#'
#'\insertRef{gupta1985statistical}{grapes}


corrApp<- function() {
  if (!requireNamespace("corrplot", quietly = TRUE)) {
    stop("Package \"corrplot\" needed for this function to work. Please install it.",
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
