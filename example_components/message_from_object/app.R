# display info from an object (e.g. a list) to a modal message window
# with a little css involved for beautification
library(shiny)

example_list <- list(session_is_active = FALSE, date = "2000-01-01", observer = "aaa", grooming_direction = NA, focal_duration = 6)

display_meta <- function(x, xcolumn) {
  if (xcolumn == 1) {
    xnames <- c("date", "observer", "focal_duration")
    out <- paste(paste0("<span style='color:grey'>", xnames, ":</span> ", x[xnames]), "<br>")
  }
  if (xcolumn == 2) {
    xnames <- c("session_is_active", "grooming_direction")
    out <- paste(paste0("<span style='color:grey'>", xnames, ":</span> ", x[xnames]), "<br>")
  }
  
  HTML(out)
}



ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      actionButton("go", "go")
    ),
    mainPanel()
  )
)

server <- function(input, output) {
  observeEvent(input$go, {
    showModal(modalDialog(
      # title = "",
      easyClose = TRUE,
      fluidRow(
        column(5, htmlOutput("out1"), style = "border: 1px solid grey; margin: 10px; padding: 2px; border-radius: 10px"), 
        column(5, htmlOutput("out2"), style = "border: 1px solid grey; margin: 10px; padding: 2px; border-radius: 10px")
      ),
      # hr(),
      fluidRow(
        column(5, htmlOutput("out3"), style = "border: 1px solid grey; margin: 10px; padding: 2px; border-radius: 10px"), 
        column(5, htmlOutput("out4"), style = "border: 1px solid grey; margin: 10px; padding: 2px; border-radius: 10px")
      )
      
    ))
    output$out1 <- renderUI(display_meta(example_list, 1))
    output$out2 <- renderUI(display_meta(example_list, 2))
    output$out3 <- renderUI(display_meta(example_list, 1))
    output$out4 <- renderUI(display_meta(example_list, 2))
  })
}

shinyApp(ui = ui, server = server)
