## HELPER FUNCTIONS ##

# A list of all symbols for this test to draw from later ---------------
all_symbols <- list(
  shiny::icon("prescription", class = "fa-xl"),
  shiny::icon("expand", class = "fa-xl"),
  shiny::icon("manat-sign", class = "fa-xl"),
  shiny::icon("ruble-sign", class = "fa-xl"),
  shiny::icon("stairs", class = "fa-xl"),
  shiny::icon("litecoin-sign", class = "fa-xl"),
  shiny::icon("lari-sign", class = "fa-xl"),
  shiny::icon("kip-sign", class = "fa-xl"),
  shiny::icon("lines-leaning", class = "fa-xl"),
  shiny::icon("hryvnia-sign", class = "fa-xl"),
  shiny::icon("guarani-sign", class = "fa-xl"),
  shiny::icon("franc-sign", class = "fa-xl"),
  shiny::icon("crop", class = "fa-xl"),
  shiny::icon("cruzeiro-sign", class = "fa-xl"),
  shiny::icon("cedi-sign", class = "fa-xl"),
  shiny::icon("asterisk", class = "fa-xl"),
  shiny::icon("envelope-open", class = "fa-xl"),
  shiny::icon("circle-half-stroke", class = "fa-xl"),
  shiny::icon("draw-polygon", class = "fa-xl")
)

# Function that gets input names for removing buttons ---------------
getInputs <- function(input, pattern){
  reactives <- names(reactiveValuesToList(input))
  reactives[grep(pattern,reactives)]
}

# Function that removes the symbol buttons ---------------
RemoveButtons <- function(input) {
  all_buttons <- c(paste0(rep("target",2),1:2),
                   paste0(rep("symbol",5),1:5),
                   "no")
  for (button_i in all_buttons) {
    vals <- paste0("#", getInputs(input, button_i))
    removeUI(
      selector = vals,
      multiple = T
    )
  }
}

# Function that reloads the icon buttons, next set ----------------
ReloadButtons <- function(my_items, current_item) {
  # seven buttons that all have one of the symbols of the current item, and
  # one button with "no"
  insertUI(
    selector = "#Buttons",
    where = "afterEnd",
    ui = fluidRow(
      actionButton(
        inputId = "target1",
        all_symbols[my_items$items[[current_item]][1]],
        class = "btn-lg",
        disabled="disabled",
        style = "color:#FBF9F6; background-color:#4c79b5"),
      actionButton(
        inputId = "target2",
        all_symbols[my_items$items[[current_item]][2]],
        class = "btn-lg",
        disabled="disabled",
        style = "color:#FBF9F6; background-color:#4c79b5"),
      actionButton(
        inputId = "symbol1",
        all_symbols[my_items$items[[current_item]][3]],
        class = "btn-lg",
        style = "color:#FBF9F6; background-color:#647b9c"),
      actionButton(
        inputId = "symbol2",
        all_symbols[my_items$items[[current_item]][4]],
        class = "btn-lg",
        style = "color:#FBF9F6; background-color:#647b9c"),
      actionButton(
        inputId = "symbol3",
        all_symbols[my_items$items[[current_item]][5]],
        class = "btn-lg",
        style = "color:#FBF9F6; background-color:#647b9c"),
      actionButton(
        inputId = "symbol4",
        all_symbols[my_items$items[[current_item]][6]],
        class = "btn-lg",
        style = "color:#FBF9F6; background-color:#647b9c"),
      actionButton(
        inputId = "symbol5",
        all_symbols[my_items$items[[current_item]][7]],
        class = "btn-lg",
        style = "color:#FBF9F6; background-color:#647b9c"),
      actionButton(
        inputId = "no",
        "No",
        class = "btn-lg",
        style = "color:#FBF9F6; background-color:#CF7041; font-type:bold")
    )
  )
}

# Function that will print the correct message ---------------
ClickButton <- function(sym_nr, output, input, my_items, current_item, correct_count) {
  if (my_items$answer_location[current_item] == sym_nr) {
    correct_count <- correct_count + 1
    if (sym_nr == 6) {
      output$answer <-
        renderText("Correct! Non of the symbols on the left is also on the right side.")
    } else {
      output$answer <-
        renderText(paste("Correct! Symbol", sym_nr, "is also on the left side."))
    }
  } else {
    if (sym_nr == 6) {
      output$answer <-
        renderText("Wrong! One of the symbols on the left also appears on the right")
    } else {
      output$answer <-
        renderText(paste("Wrong! Symbol", sym_nr, "is not on the left side."))
    }
  }
  RemoveButtons(input)
  current_item <- current_item + 1
  if (current_item <= length(my_items$items)) {
    ReloadButtons(my_items, current_item = current_item)
  }
  return(c(correct_count, current_item))
}

highlight <- function(x, value, main, xlab, col.value = "#CF7041", col=NA, ...){
  hst <- hist(x, ...)
  idx <- findInterval(value, hst$breaks)
  cols <- rep(col, length(hst$counts))
  cols[idx] <- col.value
  hist(x,
       main = main,
       xlab = xlab,
       col = cols)
}
