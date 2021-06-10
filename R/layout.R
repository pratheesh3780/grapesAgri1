#'@title Field Layout of Designs
#'
#'@description
#'layoutApp() function opens up an interactive shiny app which will allow
#'the user to create field layout of Completely Randomized Design (CRD),
#'Randomized Complete Block Design (RCBD), Split-plot design, Strip-plot design
#'and Augmented Randomized complete block design. Layout generated are
#'random. Field layout in table format can also be prepared for recording
#'observations from the field. Results can be downloaded in word, PDF or
#' HTML format
#'
#' @details
#'This app uses \code{design.crd}, \code{design.rcbd}, \code{design.dau},
#'\code{design.strip}, \code{design.split} functions of package
#'\code{agricolae} to generate random layout of designs. Field layout
#'were plotted using \code{desplot} function in \code{desplot} package.
#'To download the results in pdf one may use
#'\code{tinytex::install_tinytex()} before using the app.
#'
#' @keywords
#' Completely Randomized Design
#' Randomized complete block design
#' Split-plot design
#' Strip-plot design
#' Augmented RCBD
#'
#' @usage
#' layoutApp()
#'
#' @importFrom Rdpack reprompt
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(grapesAgri1)
#' grapesAgri1::layoutApp()
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
#'\insertRef{shinycss_2020}{grapesAgri1}
#'
#'\insertRef{dplyr_2021}{grapesAgri1}
#'
#'\insertRef{agricolae_2020}{grapesAgri1}
#'
#'\insertRef{desplot_2020}{grapesAgri1}
#'
#'\insertRef{magi_2020}{grapesAgri1}
#'
#'\insertRef{Yihui_Xie_2021}{grapesAgri1}
#'
#'\insertRef{gupta1985statistical}{grapesAgri1}
#'
#'\insertRef{das1979design}{grapesAgri1}

layoutApp<- function() {
  if (!requireNamespace("desplot", quietly = TRUE)) {
    stop("Package \"desplot\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  appDir <- system.file("layout", package = "grapesAgri1")
  if (appDir == "") {
    stop("Could not find directory. Try re-installing `grapesAgri1`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
