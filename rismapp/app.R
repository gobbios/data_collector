# this is the admin/analysis app...
library(shiny)
library(rhandsontable)

all_observers <- c("findus", "petterson", "maria", "carel", "jeanne", "robert", "joan", "julia")
all_groups <- c("pb", "r1")

ui <- fluidPage(
  navbarPage("analyse me data collada", id = "nav_home",
             tabPanel("overview",
                      sidebarLayout(
                        sidebarPanel(
                          HTML("loading data from..."),
                          dateInput("date_from", "from"),
                          dateInput("date_to", "to"),
                          checkboxGroupInput("group_sel", "group(s)", choices = all_groups),
                          checkboxGroupInput("observer_sel", "observer(s)", choices = all_observers),
                        ),
                        
                        mainPanel(
                          tabsetPanel(
                            tabPanel("overview", rHandsontableOutput("collectionlog")),
                            tabPanel("focal sessions", rHandsontableOutput("sessionlog")),
                            tabPanel("swellings", rHandsontableOutput("swellings"))
                          )
                          
                        )
                      )
             
             ),
             navbarMenu("swelling", 
                        tabPanel(title = "mismatches",
                                 rHandsontableOutput("swellings_mismatches")
                                 
                        )
                        )
             
             
  )

)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # load all data...
  allfiles <- normalizePath(list.files(c("../www", "www"), include.dirs = TRUE, full.names = TRUE))
  collection_paths <- allfiles

  # read collection log
  collectionlogs <- read_collection_summary(collection_paths = collection_paths)
  Sys.sleep(0.5)
  # read session logs
  sessionlogs <- read_session_log_files(collection_paths = collection_paths)
  Sys.sleep(0.5)
  # read focal data
  focalsessions <- read_focalsession_files(collection_paths = collection_paths)
  Sys.sleep(0.5)
  # read census data
  censusdata <- read_census_files(collection_paths = collection_paths)
  Sys.sleep(0.5)
  
  # update selections
  updateCheckboxGroupInput(inputId = "group_sel", selected = unique(collectionlogs$group))
  updateCheckboxGroupInput(inputId = "observer_sel", selected = unique(collectionlogs$observer))
  updateDateInput(inputId = "date_from", value = min(collectionlogs$date))
  updateDateInput(inputId = "date_to", value = max(collectionlogs$date))
  
  
  # make reactive objects
  xdata <- reactiveValues(collectionlogs = collectionlogs, census = censusdata, trigger = runif(1))
  
  # select
  observeEvent(input$date_from, xdata$trigger <- runif(1))
  observeEvent(input$date_to, xdata$trigger <- runif(1))
  observeEvent(input$group_sel, xdata$trigger <- runif(1))
  observeEvent(input$observer_sel, xdata$trigger <- runif(1))
  
  observeEvent(xdata$trigger, {
    sel <- which(collectionlogs$date <= input$date_to & collectionlogs$date >= input$date_from & collectionlogs$group %in% input$group_sel & collectionlogs$observer %in% input$observer_sel)
    xdata$collectionlogs <- collectionlogs[sel, ]
    sel <- which(censusdata$date <= input$date_to & censusdata$date >= input$date_from ) #& censusdata$group %in% input$group_sel & censusdata$observer %in% input$observer_sel
    xdata$census <- censusdata[sel, ]
  })
  
  output$collectionlog <- renderRHandsontable(rhandsontable(xdata$collectionlogs, rowHeaders = FALSE, readOnly = TRUE, digits = 1))

  output$swellings_mismatches <- renderRHandsontable({
    out <- swelling_mismatches(xdata$census)
    if (!isFALSE(out)) {
      out <- rhandsontable(out, rowHeaders = FALSE, readOnly = TRUE, digits = 1)
      return(out)
    }
  })
  
  
  
  
  output$focalsessions <- renderRHandsontable(rhandsontable(focalsessions, rowHeaders = FALSE, readOnly = TRUE))
  
  
  # output$sessionlog <- renderRHandsontable(rhandsontable(sessionlogs, rowHeaders = FALSE))
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
