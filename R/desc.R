#'@title descAPP for Descriptive statistics and Visualization
#'@description
#'descApp() function opens up an interactive shiny app which will allow
#'the user to easily calculate Summary Statistics, Summary Statistics by Group, Box plot,
#'Histogram, Q-Q plot and Shapiro-Wilk's test by uploading CSV file.
#' @details
#'This app uses \code{descr} and \code{stby} functions of \code{summarytools}
#'package (Dominic Comtois, 2021) to calculate summary statistics and
#'summary statistics by group. \code{knitr} (Yihui Xie,2021) and \code{kableExtra}(Hao Zhu,2021) packages
#'were used to produce HTML tables. \code{shapiro.test}, \code{qqnorm} and \code{qqline} functions of
#'\code{stats}  package were used for Test of Homogeneity of variance and obtaining
#'Q-Q plot. \code{hist} and \code{boxplot} of package \code{graphics} were used
#'to obtain histogram and boxplot respectively. \code{ggqqplot} of package \code{ggpubr} (Alboukadel Kassambara,2020)
#'is also used to plot Q-Q plot in the app.
#' @keywords
#' descriptive statistics
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
