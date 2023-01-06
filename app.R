# this is the final app...

library(shiny)
library(rhandsontable)


# source("helpers/adlib_aggression_dyadic.R")
# source("helpers/empty_foc_table.R")


# individual table 
all_individuals <- read.csv("id_table.csv", stringsAsFactors = FALSE)
all_individuals$present <- FALSE
all_individuals$swelling <- factor(NA, levels = 1:3)
all_individuals$comment <- ""
# list with observers (to be outsourced to csv file eventually)
all_observers <- c("maria", "carel", "jeanne")



ui <- fluidPage(
  navbarPage("give me data collada", id = "nav_home",
             tabPanel("home",
                      column(2, "",
                             htmlOutput("dategroupobs")
                      ),
                      column(10, "",
                             actionButton(inputId = "start_focal_btn", label = "start focal session", style = "background: rgba(255, 0, 0, 0.5); height:100px")
                      )
             ),

             tabPanel("census",
                      rHandsontableOutput("hot"),
                      actionButton("addnewrowtocensus", "add new row")
             )
             
  )
)


server <- function(input, output, session) {
  xdata <- reactiveValues(presence = all_individuals, get_started = FALSE)
  
  showModal(modalDialog(title = "hello there, what's up today?",
    dateInput("date", "date"),
    selectInput("observer", "observer", choices = unique(all_observers)),
    selectInput("group", "group", choices = c(unique(all_individuals$group))),
    footer = tagList(
      # modalButton("Cancel"),
      actionButton("startnewday_ok", "OK"),
      HTML("<p style='color:Khaki;'>to be done: 'are you sure?'-button")
    )
  ))
  observeEvent(input$startnewday_ok, {
    xdata$presence <- xdata$presence[xdata$presence$group == input$group, ] # select relevant group...
    xdata$get_started <- TRUE
    output$dategroupobs <- renderText({
      paste("<p>selected group:", "<b>", input$group, "</b></p>", "<hr>", 
            "<p>selected date:<b>", as.character(input$date), "</b></p>", "<hr>", 
            "<p>selected observer:<b>", as.character(input$observer), "</b></p>")
    })
    removeModal()
  })
  observe({
    hot = isolate(input$hot)
    if (!is.null(hot)) {
      if (nrow(hot_to_r(input$hot)) > 0) {
        xdata$presence$present <- hot_to_r(input$hot)$present
      }
    }
  })
  observeEvent(input$addnewrowtocensus, {
    if (!is.null(xdata$presence) & xdata$get_started == TRUE) {
      xdata$presence <- rbind(xdata$presence, NA)
      xdata$presence$present[nrow(xdata$presence)] <- FALSE
    }
  })
  output$hot = renderRHandsontable({
    if (!is.null(xdata$presence) & xdata$get_started == TRUE) {
      xtab <- rhandsontable(xdata$presence[, -c(which(colnames(xdata$presence) %in% c("is_focal", "sex")))], rowHeaders = NULL)
      # xtab <- hot_row(xtab, c(1,3, 5), readOnly = TRUE)
      # xtab <- hot_col(xtab, c(1), readOnly = TRUE)
      # xtab <- hot_col(xtab, c(1), readOnly = TRUE)
      hot_table(xtab, highlightCol = TRUE, highlightRow = TRUE)
    }
  })
  

}

# Run the application
shinyApp(ui = ui, server = server)
