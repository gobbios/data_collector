# 'select' a location to store files outside R's tempdir()

library(shiny)

ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
          actionButton("save_file", "save file"),
          checkboxInput("use_desktop", label = "use Desktop/temp"),
          actionButton("load", label = "load and display from selected location")
        ),

        mainPanel(
          h4("current path"),
          verbatimTextOutput("file_path"),
          h4("path to file"),
          verbatimTextOutput("file_path2"),
          hr(),
          h4("file exists in temp dir"),
          verbatimTextOutput("file_exists"),
          h4("file exists in desktop dir"),
          verbatimTextOutput("file_exists2"),
          hr(),
          br(),
          tableOutput("tab_out")
        )
    )
)

server <- function(input, output) {
  observeEvent(input$use_desktop, {
    fp <- normalizePath(tempdir())
    if (input$use_desktop) fp <- normalizePath(file.path("~/Desktop", "bla_temp"), mustWork = FALSE)
    output$file_path <- renderPrint(fp)
    if (!dir.exists(fp)) showModal(modalDialog("no 'bla_temp' directory on your Desktop (yet)"))
  })

  observeEvent(input$save_file, {
    fp <- normalizePath(tempdir())
    if (input$use_desktop) fp <- normalizePath(file.path("~/Desktop", "bla_temp"), mustWork = FALSE)
    fp <- file.path(fp, "tempout.csv")
    output$file_path2 <- renderPrint(fp)
    write.csv(head(mtcars), file = fp)
  })

  observeEvent(input$save_file, {
    output$file_exists <- renderPrint(file.exists(normalizePath(file.path(tempdir(), "tempout.csv"), mustWork = FALSE)))
    output$file_exists2 <- renderPrint(file.exists(normalizePath(file.path("~/Desktop", "bla_temp", "tempout.csv"), mustWork = FALSE)))
  })
  observeEvent(input$load, {
    fp <- normalizePath(tempdir())
    if (input$use_desktop) fp <- normalizePath(file.path("~/Desktop", "bla_temp"))
    fp <- file.path(fp, "tempout.csv")
    x <- read.csv(fp)
    output$tab_out <- renderTable(head(x))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
