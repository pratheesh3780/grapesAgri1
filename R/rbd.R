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
#' DMRT
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
#' library(grapesAgri1)
#' grapesAgri1::rbdApp()
#' }
#'
#'
#'@references
#'
#'
#'\insertRef{R_2021}{grapesAgri1}
#'
#'\insertRef{shiny_2021}{grapesAgri1}
#'
#'\insertRef{sw_2021}{grapesAgri1}
#'
#'\insertRef{dplyr_2021}{grapesAgri1}
#'
#'\insertRef{ggpubr_2020}{grapesAgri1}
#'
#'\insertRef{ggplot_2016}{grapesAgri1}
#'
#'\insertRef{gupta1985statistical}{grapesAgri1}
#'
#'\insertRef{tukey1977exploratory}{grapesAgri1}
#'
#'\insertRef{hmisc_2021}{grapesAgri1}
#'
#'\insertRef{agricolae_2020}{grapesAgri1}
#'
#'\insertRef{rcol_2014}{grapesAgri1}
#'
#'\insertRef{shinycss_2020}{grapesAgri1}
#'
#'\insertRef{das1979design}{grapesAgri1}

rbdApp<- function() {
  appDir <- system.file("RBD", package = "grapesAgri1")
  if (appDir == "") {
    stop("Could not find directory. Try re-installing `grapesAgri1`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
