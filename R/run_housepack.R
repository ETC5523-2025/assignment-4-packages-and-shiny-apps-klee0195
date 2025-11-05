#' Launch the HousePack Shiny App
#'
#' Opens the interactive dashboard that explores housing prices,
#' income per capita, and capital gains across Australia.
#'
#' @export
run_housepack <- function() {
  app_dir <- system.file("shiny-app", package = "housepack")
  shiny::runApp(app_dir, display.mode = "normal")
}
