#'@title descAPP for Descriptive statistics and Visualization
#'@description
#'descApp() function opens up a shiny app which will allow you to easily
#'calculate Summary Statistics, Summary Statistics by Group, Box plot,
#'Histogram, Q-Q plot and Shapiro-Wilk's test by uploading CSV file.
#' @details
#' In this Shiny App, you only just need to upload the Excel file in CSV format.
#' To Prepare excel file follow the instructions below
#' Open a new blank excel file.
#' Copy and paste observations into a new sheet (use only one sheet) of a new excel file.
#' Observations should be pasted as columns.
#' Don't type or delete anything on other cells without data.
#' You can use any names for your columns. No space is allowed in the Column name. If space is required use underscore '_' or '.' full stop; for example 'Variable name' should be written as Variable_name or Variable.name.
#' Data should be arranged towards upper left corner and row above the data should not be left blank.
#' Don't type and delete anything on other cells without data. If so select those cells, right click and click clear contents.
#' Give names to all column, Don't add any unnecessary columns that is not required for analysis
#' Once all these are done, your file is ready. Now you have to save it as CSV file.
#' @keywords
#' descrpitive statistics
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
