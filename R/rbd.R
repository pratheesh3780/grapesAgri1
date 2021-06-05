#'@title Randomized Block Design
#'
#'@description
#'rbdApp() function opens up an interactive shiny app which will allow
#'the user to perform analysis of randomized Block design. 
#'Multiple comparison tests like LSD,DMRT and Tukey can be performed. 
#'Box-pot and Bar-chart with confidence interval
#'can be plotted. All these can be achieved by uploading CSV file.
#'
#' @details
#'This app uses \code{anova} function of \code{stats} package to
#'obtain two-way ANOVA.\code{LSD.test},\code{duncan.test} and
#'\code{HSD.test} functions of \code{agricolae} package is used for
#'multiple comparison test like LSD,DMRT and Tukey respectively.
#'\code{ggboxplot} function of \code{ggpubr} package is used for
#'boxplot.'\code{ggplot} function of \code{ggplot2} is used for
#'barchart with confidence interval. To download the results in pdf
#'one may use \code{tinytex::install_tinytex()} before using the app.
#'
#' @keywords
#' Two-way ANOVA
#' Randomized Block Design
#' Multiple comparison Tests
#' LSD, Tukey, DMRT
#' Box plot
#' Barchart with confidence Interval
#'
#' @usage
#' rbdApp()
#'
#' @importFrom Rdpack reprompt
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(grapes)
#' grapes::rbdAPP()
#' }
#'
#'
#'@references
#'
#'
#'\insertRef{R_2021}{grapes}
#'
#'\insertRef{shiny_2021}{grapes}
#'
#'\insertRef{sw_2021}{grapes}
#'
#'\insertRef{dplyr_2021}{grapes}
#'
#'\insertRef{ggpubr_2020}{grapes}
#'
#'\insertRef{ggplot_2016}{grapes}
#'
#'\insertRef{gupta1985statistical}{grapes}
#'
#'\insertRef{tukey1977exploratory}{grapes}
#'
#'\insertRef{hmisc_2021}{grapes}
#'
#'\insertRef{agricolae_2020}{grapes}
#'
#'\insertRef{rcol_2014}{grapes}
#'
#'\insertRef{shinycss_2020}{grapes}
#'
#'\insertRef{das1979design}{grapes}
#'
rbdApp<- function() {
  appDir <- system.file("RBD", package = "grapes")
  if (appDir == "") {
    stop("Could not find directory. Try re-installing `grapes`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}
