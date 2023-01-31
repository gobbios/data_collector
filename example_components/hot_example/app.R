# handsontable example
# - with checkbox
# - add new row via button

library(shiny)
library(rhandsontable)
allx <- read.csv("../../id_table.csv", stringsAsFactors = FALSE)
allx$present <- FALSE
allx$swelling <- factor(NA, levels = 1:3)

ui <- fluidPage(
  actionButton("addnewrow", "add new row"),
  rHandsontableOutput("hot"),
  h4("static table"),
  tableOutput("standardtab"),
  textOutput("realgroup")
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  xdata <- reactiveValues(presence = allx, get_started = FALSE)

  showModal(modalDialog(
    selectInput("group", "group", choices = c(unique(allx$group))),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("ok", "OK")
    )
  ))
  
  
  observeEvent(input$ok, {
    xdata$presence <- xdata$presence[xdata$presence$group == input$group, ]
    xdata$get_started <- TRUE
    removeModal()
  })
  output$realgroup <- renderPrint(input$group)

  
  observe({
    hot = isolate(input$hot)
    if (!is.null(hot)) {
      if (nrow(hot_to_r(input$hot)) > 0) {
        xdata$presence$present <- hot_to_r(input$hot)$present
      }
    }
  })
  
  observeEvent(input$addnewrow, {
    if (!is.null(xdata$presence) & xdata$get_started == TRUE) {
      xdata$presence <- rbind(xdata$presence, NA)
      xdata$presence$present[nrow(xdata$presence)] <- FALSE
    }
  })
  output$hot = renderRHandsontable({
    if (!is.null(xdata$presence) & xdata$get_started == TRUE) {
      xtab <- rhandsontable(xdata$presence, rowHeaders = NULL)
      # xtab <- hot_row(xtab, c(1,3, 5), readOnly = TRUE)
      xtab <- hot_col(xtab, c(1), readOnly = TRUE)
      xtab <- hot_col(xtab, c(1), readOnly = TRUE)
      hot_table(xtab, highlightCol = TRUE, highlightRow = TRUE)
    }
  })
  
  output$standardtab <- renderTable({
    # xdata$presence[xdata$presence$present, ]
    # xdata$presence[hot_to_r(input$hot)$present, ]
    hot_to_r(input$hot)[hot_to_r(input$hot)$present, ]
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
