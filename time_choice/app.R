# set up a time widget, which displays the minute following the current time
# and can be increased/decreased in one-minute intervals via action buttons

curtime <- function() {
  x <- unlist(as.POSIXlt(Sys.time()))
  h <- as.numeric(x["hour"])
  m <- as.numeric(x["min"])
  s <- ceiling(as.numeric(x["sec"]))
  
  if (s > 30) {
    m <- m + 1
  }
  if (m == 60) {
    m <- 0
    h <- h + 1
  }
  
  c(h, m)
}

library(shiny)

ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
          splitLayout(cellWidths = c("20%", "15%", "20%", "15%"), 
                      textInput("time_val", label = NULL, width = 80), 
                      actionButton("decr", "", icon = icon("circle-minus")),
                      htmlOutput("text_time"),
                      actionButton("incr", "", icon = icon("circle-plus"))
                      ),
          
          hr(),
          actionButton("start", "start_session")
        ),
        mainPanel()
    )
)

server <- function(input, output) {
  time_for_start <- reactiveValues(hour = NULL, minute = NULL)
  
  observeEvent(input$start, {
    time_for_start$hour <- curtime()[1]
    time_for_start$minute <- curtime()[2]
    updateTextInput(inputId = "time_val", value = paste0(sprintf("%02.f", time_for_start$hour), 
                                                         ":", 
                                                         sprintf("%02.f", time_for_start$minute)))
  })
  observe( {
    if (!is.null(time_for_start$hour) & !is.null(time_for_start$minute)) {
      x <- paste0(sprintf("%02.f", time_for_start$hour), 
                  ":", 
                  sprintf("%02.f", time_for_start$minute))
      output$text_time <- renderUI(HTML(paste(x)))
    }
  })
  
  observeEvent(input$incr, {
    xxx <- unlist(strsplit(input$time_val, ":"))
    x <- as.numeric(xxx[2])
    y <- as.numeric(xxx[1])
    if (is.na(x)) x <- curtime()[2]
    if (is.na(y)) y <- curtime()[1]
    if (x > 59 | x < 0) x <- curtime()[2]
    if (y > 23 | y < 0) y <- curtime()[1]
    
    x <- x + 1
    if (x == 60) {
      x <- 0
      y <- y + 1
    }
    time_for_start$minute <- x
    time_for_start$hour <- y
    updateTextInput(inputId = "time_val", value = paste0(sprintf("%02.f", time_for_start$hour), 
                                                         ":", 
                                                         sprintf("%02.f", time_for_start$minute)))
    
  })
  observeEvent(input$decr, {
    xxx <- unlist(strsplit(input$time_val, ":"))
    x <- as.numeric(xxx[2])
    y <- as.numeric(xxx[1])
    if (is.na(x)) x <- curtime()[2]
    if (is.na(y)) y <- curtime()[1]
    if (x > 59 | x < 0) x <- curtime()[2]
    if (y > 23 | y < 0) y <- curtime()[1]
    
    x <- x - 1
    if (x == -1) {
      x <- 59
      y <- y - 1
    }
    time_for_start$minute <- x
    time_for_start$hour <- y
    updateTextInput(inputId = "time_val", value = paste0(sprintf("%02.f", time_for_start$hour), 
                                                         ":", 
                                                         sprintf("%02.f", time_for_start$minute)))
  })
}

shinyApp(ui = ui, server = server)
