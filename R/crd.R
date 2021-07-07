#' @title Completely Randomized Design
#' @return Nothing
#' @description
#' crdApp() function opens up an interactive shiny app which will allow
#' the user to perform analysis of completely randomized design with
#' equal or unequal replications. Multiple comparison tests like LSD,
#' DMRT and Tukey can be performed. Box-pot and Bar-chart with confidence interval
#' can be plotted. All these can be achieved by uploading CSV file.
#'
#' @details
#' This app uses \code{anova} function of \code{stats} package to
#' obtain one-way ANOVA.\code{LSD.test},\code{duncan.test} and
#' \code{HSD.test} functions of \code{agricolae} package is used for
#' multiple comparison test like LSD,DMRT and Tukey respectively.
#' \code{ggboxplot} function of \code{ggpubr} package is used for
#' boxplot.'\code{ggplot} function of \code{ggplot2} is used for
#' barchart with confidence interval.
#'
#' @keywords
#' One-way ANOVA
#' Completely Randomized Design
#' Multiple comparison Tests
#' DMRT
#' Box plot
#' Barchart with confidence Interval
#'
#' @usage
#' crdApp()
#'
#' @importFrom Rdpack reprompt
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   crdApp()
#' }
#' @references
#'
#'
#' \insertRef{R_2021}{grapesAgri1}
#'
#' \insertRef{shiny_2021}{grapesAgri1}
#'
#' \insertRef{sw_2021}{grapesAgri1}
#'
#' \insertRef{dplyr_2021}{grapesAgri1}
#'
#' \insertRef{ggpubr_2020}{grapesAgri1}
#'
#' \insertRef{ggplot_2016}{grapesAgri1}
#'
#' \insertRef{gupta1985statistical}{grapesAgri1}
#'
#' \insertRef{tukey1977exploratory}{grapesAgri1}
#'
#' \insertRef{hmisc_2021}{grapesAgri1}
#'
#' \insertRef{agricolae_2020}{grapesAgri1}
#'
#' \insertRef{rcol_2014}{grapesAgri1}
#'
#' \insertRef{shinycss_2020}{grapesAgri1}
#'
#' \insertRef{das1979design}{grapesAgri1}
#'

crdApp <- function() {
  appDir <- system.file("crd", package = "grapesAgri1")
  if (appDir == "") {
    stop("Could not find directory. Try re-installing `grapesAgri1`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
