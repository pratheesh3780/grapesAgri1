#'@title t-test and Paired t-test
#'@description
#'ttApp() function opens up an interactive shiny app which will allow
#'user to easily perform one sample t-test, unpaired two sample t-test
#'unpaired two sample Welch t-test, paired t-test, test for homogeneity of variance (F-test),
#'and obtain plots like boxplot and paired plot by uploading CSV file.
#'@details
#'This app uses \code{t.test} function to calculate t statistic.Descriptive statistics
#'were calculated using \code{stat.desc} function of \code{pastecs} package.
#'\code{var.test} function is used for F-test.\code{ggboxplot} function
#'of \code{ggpubr} package is used to draw boxplot. Paired plot is obtained
#'using \code{paired} function of package \code{PairedData}
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
#'
#'@references
#'
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
#'\insertRef{past_2018}{grapes}
#'
#'\insertRef{gupta1985statistical}{grapes}
#'
#'\insertRef{paired_data2018}{grapes}



ttApp<- function() {

  if (!requireNamespace("reshape2", quietly = TRUE)) {
    stop("Package \"reshape2\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("PairedData", quietly = TRUE)) {
    stop("Package \"PairedData\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  appDir <- system.file("comp_mean", package = "grapes")
  if (appDir == "") {
    stop("Could not find directory. Try re-installing `grapes`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
