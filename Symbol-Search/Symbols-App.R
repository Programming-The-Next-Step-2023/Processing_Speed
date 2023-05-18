#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#

library(shiny)
library("fontawesome")

ui <- fluidPage(
  titlePanel("Symbol Search"),
  mainPanel(
    "Is either one of the two symbols on the left, also on the right side?",
    width = 20),
  actionButton(
    inputId = "symbol1",
    all_symbols[my_items[["items"]][["item_1"]][1]],
    class = "btn-lg btn-primary",
    disabled="disabled"),
  actionButton(
    inputId = "symbol2",
    all_symbols[my_items[["items"]][["item_1"]][2]],
    class = "btn-lg btn-primary",
    disabled="disabled"),
  actionButton(
    inputId = "symbol3",
    all_symbols[my_items[["items"]][["item_1"]][3]],
    class = "btn-lg btn-info"),
  actionButton(
    inputId = "symbol4",
    all_symbols[my_items[["items"]][["item_1"]][4]],
    class = "btn-lg btn-info"),
  actionButton(
    inputId = "symbol5",
    all_symbols[my_items[["items"]][["item_1"]][5]],
    class = "btn-lg btn-info"),
  actionButton(
    inputId = "symbol6",
    all_symbols[my_items[["items"]][["item_1"]][6]],
    class = "btn-lg btn-info"),
  actionButton(
    inputId = "symbol7",
    all_symbols[my_items[["items"]][["item_1"]][7]],
    class = "btn-lg btn-info"),
  actionButton(
    inputId = "no",
    "No",
    class = "btn-lg btn-danger"),
)

server <- function(input, output, session) {
}

# Run the application
shinyApp(ui, server)
