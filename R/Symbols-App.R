#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
#'@import shiny
#'@import lubridate
#'@import bslib
#'
require(shiny)
require(lubridate)
require(bslib)

# First generate some items using the GenerateItems function from this package
my_items <- GenerateItems(60)
# Then set the item & correct item counter to 1 & 0 as the start values
current_item <- reactiveVal(1)
correct_count <- reactiveVal(0)
# Initialize the timer, 60 seconds, starts as not active.
timer_setting <- 60
timer <- reactiveVal(timer_setting)
active <- reactiveVal(FALSE)

# the UI
  ui <- fluidPage(
    # set the theme
    theme = bslib::bs_theme(
      version = "3",
      bg = "#e9edf0", fg = "#132f42",
      bootswatch = "flatly",
      heading_font = bslib::font_collection(bslib::font_google("Schneidler Blk BT", local = FALSE), "Futura"),
      base_font = bslib::font_collection(bslib::font_google("Roboto", local = FALSE), "Roboto", "sans-serif")
    ),
    # first make a header of the task
    span(titlePanel("Symbol Search"),
         style = "color:#A7562f"),
    # add an explanation of the task with a set width to increase readability
    fluidRow(column(width = 8,
           uiOutput(outputId = "explanation")
           ), style = "margin-bottom: 7px;"),

    # set the place where the buttons will be added
    column(width = 12, uiOutput("Buttons")),

    # create a "start" button for the player to click to start the task
    fluidRow(column(width = 12, offset = 3,
    actionButton("start", "START", width = "10%",
                 style = "color:#FBF9F6;
                          background-color:#647b9c;
                          font-style:bold;
                          margin-top: 20px;"))),

    # Add the different output places where the correct/incorrect info and
    # end page info will be placed
    fluidRow(column(width = 12, style = "margin-top: 7px;",
                    textOutput("answer"),
                    textOutput("item_inform"),
                    span(textOutput("end_page"),
                         style = "color:#4c79b5;
                                  font-size:30px;
                                  font-family:Schneidler Blk BT"),
                    textOutput("score")
                    )),

    conditionalPanel("input.save==0",
      span(textOutput("timeleft"),style = "color:#cf571d; font-type:bold"),
      uiOutput("save")
      ),
    plotOutput("plot")
    )

  server <- function(input, output, session) {
    # Explanation of task to reader
    output$explanation <- renderText("In this task, you will get to see rows
                                     consisting of 2 target symbols and 5 search
                                     symbols. Indicate for each item whether one
                                     of the target symbols also occurs in the
                                     search symbols by clicking on this symbol.
                                     If non of the target symbols occurs in the
                                     search symbols, click on the button '<b>no</b>'.
                                     You have 1 minute to do as many items as
                                     possible. The timer starts when you click
                                     on <b>start</b>.")


    # observer that invalidates every second.
    observe({
      invalidateLater(1000, session)
      isolate({
        if(active())  # If timer is active, decrease by one.
        {
          timer(timer()-1)
          # end task if the timer runs out (or if they answered all the items)
          if(timer()<1 | current_item() > length(my_items$items))
          {
            active(FALSE)
            RemoveButtons(input)
            # Inform about end score in dialog box
            showModal(modalDialog(
              title = "~ End of the Symbol Search Task ~",
              paste(ifelse(timer()<1, "Time is up!","You answered all items!"),
                    "You had", correct_count(), "out of",
                    current_item()-1, "items correct")
            ))
            # Remove irrelevant text
            output$answer <- renderText("")
            output$explanation <- renderText("")
            # Inform about end score on final screen
            output$end_page <- renderText(" ~ The End ~")
            output$score <- renderText(paste("You had", correct_count(),
                                             "out of", current_item()-1 ,
                                             "items correct!"))
            insertUI(
              selector = "#save",
              where = "afterEnd",
              ui = actionButton(
              inputId = "save",
              label = "Save & See My Results",
              class = "btn",
              style = "color:#FBF9F6;
                       background-color:#E2A55E;
                       margin-top: 20px")
            )
          }
        }
      })
    })

    # When start button is pressed, start printing timer, print short
    # explanation, load buttons and start timer by turning active to TRUE
    observeEvent(input$start, {
      # Output the time left.
      output$timeleft <- renderText({
        paste("Time left: ", lubridate::seconds_to_period(timer()))
      })
      output$explanation <-
        renderText("Is either of the target symbols also on the right side?")
      ReloadButtons(my_items, current_item = current_item())
      removeUI(
        selector = paste0("#", getInputs(input, "start")),
        multiple = T
      )
      active(TRUE)
    })

    # When any one of the buttons is pressed, the ClickButton function is
    # called and user will be informed about their answer. Also this function
    # updates the correct counter and current item counter accordingly (correct
    # counter will increase if item was correct and the current item counter
    # increases with every click).
    observeEvent(input$symbol1, {
      counters <- ClickButton(sym_nr = 1, output, input, my_items,
                              current_item=current_item(),
                              correct_count=correct_count())
      correct_count(counters[1])
      current_item(counters[2])
      })
    observeEvent(input$symbol2, {
      counters <- ClickButton(sym_nr = 2, output, input, my_items,
                              current_item=current_item(),
                              correct_count=correct_count())
      correct_count(counters[1])
      current_item(counters[2])
    })
    observeEvent(input$symbol3, {
      counters <- ClickButton(sym_nr = 3, output, input, my_items,
                              current_item=current_item(),
                              correct_count=correct_count())
      correct_count(counters[1])
      current_item(counters[2])
    })
    observeEvent(input$symbol4, {
      counters <- ClickButton(sym_nr = 4, output, input, my_items,
                              current_item=current_item(),
                              correct_count=correct_count())
      correct_count(counters[1])
      current_item(counters[2])
    })
    observeEvent(input$symbol5, {
      counters <- ClickButton(sym_nr = 5, output, input, my_items,
                              current_item=current_item(),
                              correct_count=correct_count())
      correct_count(counters[1])
      current_item(counters[2])
    })
    observeEvent(input$no, {
      counters <- ClickButton(sym_nr = 6, output, input, my_items,
                              current_item = current_item(),
                              correct_count = correct_count())
      correct_count(counters[1])
      current_item(counters[2])
    })

    observeEvent(input$save, {
      # First saving the data from the current run
      to_save <- list(correct = correct_count(),
                       total_items = current_item() - 1,
                       time = timer_setting - timer())
      scores <- readRDS(file = "~/GitHub/ProcessingSpeed/data/scores.rds")
      to_save$total_items <- c(to_save$total_items, scores$total_items)
      to_save$correct <- c(to_save$correct, scores$correct)
      to_save$time <- c(to_save$time, scores$time)
      saveRDS(to_save, file = "~/GitHub/ProcessingSpeed/data/scores.rds")
      # Then using the data to visualize the score of the current person
      # in comparison with previous scores
      scores_df <- as.data.frame(to_save)

      # Giving the option to choose the type of graph displayed
      insertUI(
        selector = "#plot",
        where = "beforeBegin",
        ui = selectInput(
            inputId = "choose_plot",
            label = "Choose a Plot",
            selected = "",
            choices = list("Total Score" = "total_score",
                           "Total Items per Minute" = "correct",
                           "Total Items Incorrect" = "incorrect",
                           "Seconds per Correct Item" = "seconds_per_correct")
            )
        )

      # calculating the right scores to make histograms of
      scores_df$seconds_per_correct <- scores_df$time/scores_df$correct
      scores_df$incorrect <- scores_df$total_items-scores_df$correct
      scores_df$total_score <- scores_df$correct-scores_df$incorrect

      # making list with names for the x-axis of the plots
      plot_names <- list("total_score" ="Total Score (Items Correct - Items Incorrect)",
                      "correct" = "Total Number of Correct Items per Minute",
                      "incorrect" = "Total Number of Incorrect Items per Minute",
                      "seconds_per_correct" = "Number of Seconds per Item")

      # When "choose_plot" changes, display the plot that they have selected
      observeEvent(input$choose_plot, {
                    output$plot <- renderPlot({
                      par(bg="#e9edf0")
                      highlight(x = scores_df[[input$choose_plot]],
                                value = scores_df[[input$choose_plot]][1],
                                main = "Distribution of Scores",
                                xlab = plot_names[[input$choose_plot]])
                      legend(x ="topright", legend = "Your Score", fill = "#CF7041", bty = "n")
                      })
                    }

      )
    })
  }
