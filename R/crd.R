#'@title Completely Randomized Design
#'
#'@description
#'crdApp() function opens up an interactive shiny app which will allow
#'the user to perform analysis of completely randomized design with
#'equal or unequal replications. Multiple comparison tests like LSD,
#'DMRT and Tukey can be performed. Box-pot and Bar-chart with confidence interval
#'can be plotted. All these can be achieved by uploading CSV file.
#'
#' @details
#'This app uses \code{descr} and \code{stby} functions of \code{summarytools}
#'package (Dominic Comtois, 2021) to calculate summary statistics and
#'summary statistics by group. \code{knitr} (Yihui Xie,2021) and \code{kableExtra}(Hao Zhu,2021) packages
#'were used to produce HTML tables. \code{shapiro.test}, \code{qqnorm} and \code{qqline} functions of
#'\code{stats}  package were used for Test of Homogeneity of variance and obtaining
#'Q-Q plot. \code{hist} and \code{boxplot} of package \code{graphics} were used
#'to obtain histogram and boxplot respectively. \code{ggqqplot} of package \code{ggpubr} (Alboukadel Kassambara,2020)
#'is also used to plot Q-Q plot in the app.
#'
#' @keywords
#' One-way ANOVA
#' Completely Randomized Design
#' Multiple comparison Tests
#' LSD, Tukey, DMRT
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
#' \dontrun{
#' library(grapes)
#' grapes::crdAPP()
#' }
#'
#'
#'@references
#'
#'
#'
#'\insertRef{Dominic_Comtois_2021}{grapes}
#'
#'\insertRef{Hao_zhu_2021}{grapes}
#'
#'\insertRef{Yihui_Xie_2021}{grapes}
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
#'\insertRef{past_2018}{grapes}
#'
#'\insertRef{magi_2020}{grapes}
#'
#'\insertRef{gridG_2020}{grapes}
#'
#'\insertRef{gupta1985statistical}{grapes}
#'
#'\insertRef{tukey1977exploratory}{grapes}
#'
#'\insertRef{ggplot_2016}{grapes}


crdApp<- function() {
  appDir <- system.file("crd", package = "grapes")
  if (appDir == "") {
    stop("Could not find directory. Try re-installing `grapes`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
