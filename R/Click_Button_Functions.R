getInputs <- function(input, pattern){
  reactives <- names(reactiveValuesToList(input))
  reactives[grep(pattern,reactives)]
}

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

# Function that will print the correct message based on whether the item
# was answered correctly or not
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
