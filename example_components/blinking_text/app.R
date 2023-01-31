library(shiny)

ui <- fluidPage(
  tags$head(
    # Note the wrapping of the string in HTML()
    tags$style(HTML("
    @keyframes blinker {
    0% { background-color: white; }
    50% { background-color: rgba(255, 0, 0, 0.5); }
    100% { background-color: white; }
    }
    .blink_me { animation: blinker 5s linear infinite; }
"))
  ),

    sidebarLayout(
        sidebarPanel(
        ),
        mainPanel(
          checkboxInput("toggle", "toggle"),
          tagAppendAttributes(tag = textOutput("bbb"), class = 'blink_me')
          )
    )
)

server <- function(input, output) {
    output$bbb <- renderText("blub")
}

shinyApp(ui = ui, server = server)
