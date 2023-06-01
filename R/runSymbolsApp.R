#' Run the Symbol Search Task App
source("R/Helper_Functions.R")
source("R/GenerateItems.R")
source("R/Symbols-App.R")
#' runSymbolsApp
#'
#' @description
#' The function that can be run to start the Symbol Search Task App.
#'
#' @examples runSymbolsApp()
#'
#' @export
runSymbolsApp <- function() {
  shiny::shinyApp(ui = ui, server = server)
}

runSymbolsApp()
