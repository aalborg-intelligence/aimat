#' Run an aimat Shiny app
#'
#' This function launches a Shiny app for training neural networks using the `aimat` package.
#' @param appname A character string specifying the name of the app to run. The app should be located in the `aimat` package's system file directory.
#'
#' @examples
#' if (interactive()) {
#'   aimatApp("app_full")
#' }
#' @export
aimatApp <- function(appname) {
  appDir <- system.file(appname, package = "aimat")
  if (appDir == "") {
    stop("Could not find app directory.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
