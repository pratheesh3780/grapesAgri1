#'@title Descriptive statistics and Visualization
#'@return Nothing
#'@description
#'descApp() function opens up an interactive shiny app which will allow
#'the user to easily calculate Summary Statistics, Summary Statistics by Group, Box plot,
#'Histogram, Q-Q plot and Shapiro-Wilk's test by uploading CSV file.
#'
#'@details
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
#' descriptive statistics
#' histogram
#' q-q plot
#' box plot
#' summary statistics
#' summary statistics by group
#'
#' @usage
#' descApp()
#'
#' @importFrom Rdpack reprompt
#'
#' @export
#'
#' @examples
#'if(interactive()){
#'descApp()
#'}
#'
#'
#'@references
#'
#'
#'
#'\insertRef{Dominic_Comtois_2021}{grapesAgri1}
#'
#'\insertRef{Hao_zhu_2021}{grapesAgri1}
#'
#'\insertRef{Yihui_Xie_2021}{grapesAgri1}
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
#'\insertRef{past_2018}{grapesAgri1}
#'
#'\insertRef{magi_2020}{grapesAgri1}
#'
#'\insertRef{gridG_2020}{grapesAgri1}
#'
#'\insertRef{gupta1985statistical}{grapesAgri1}
#'
#'\insertRef{tukey1977exploratory}{grapesAgri1}
#'
#'\insertRef{ggplot_2016}{grapesAgri1}


descApp<- function() {
  if (!requireNamespace("kableExtra", quietly = TRUE)) {
    stop("Package \"kableExtra\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("summarytools", quietly = TRUE)) {
    stop("Package \"summarytools\" needed for this function to work. Please install it.",
         call. = FALSE)
  }


  appDir <- system.file("desc", package = "grapesAgri1")
  if (appDir == "") {
    stop("Could not find directory. Try re-installing `grapesAgri1`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
