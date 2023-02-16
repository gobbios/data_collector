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
                            tabPanel(rHandsontableOutput("sessionlog"))
                          )
                          
                        )
                      )
             )# ,
             # tabPanel("focal sessions",
             #          sidebarLayout(
             #            sidebarPanel(
             #              HTML("loading data from..."),
             #              dateInput("date_from", "from"),
             #              dateInput("date_to", "to"),
             #              checkboxGroupInput("group_sel", "group(s)", choices = all_groups),
             #              checkboxGroupInput("observer_sel", "observer(s)", choices = all_observers),
             #            ),
             #            
             #            mainPanel(
             #              tabsetPanel(
             #                tabPanel("focal sessions", rHandsontableOutput("focalsessions"))
             #                
             #              )
             #              
             #            )
             #          )
             #          )
  )

)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # load all data...
  allfiles <- normalizePath(list.files(c("../www", "../examples_sessions", "www", "examples_sessions"), include.dirs = TRUE, full.names = TRUE))
  fnames <- basename(allfiles)
  allfiles <- lapply(allfiles, list.files, full.names = TRUE)
  nfiles <- length(fnames)
  
  # read collection log
  collectionlogs <- vector(mode = "list", length = length(allfiles))
  pcounter <- 0
  withProgress(
    min = 0,
    max = nfiles,
    message = "reading collection logs",
    for (i in seq_along(allfiles)) {
      out <- read.csv(allfiles[[i]][grep("0_meta.csv", allfiles[[i]])], row.names = 1)
      collectionlogs[[i]] <- t(out[c("group", "observer", "date", "focal_sessions_so_far"), , drop = FALSE])
      pcounter <- pcounter + 1
      if (!interactive()) incProgress(pcounter)
      Sys.sleep(0.05)
    }
  )
  collectionlogs <- do.call("rbind", collectionlogs)
  collectionlogs <- data.frame(collectionlogs)
  collectionlogs$date <- as.Date(collectionlogs$date)
  collectionlogs$focal_sessions_so_far <- as.integer(collectionlogs$focal_sessions_so_far)
  rownames(collectionlogs) <- NULL
  
  # read session logs
  sessionlogs <- vector(mode = "list", length = length(allfiles))
  pcounter <- 0
  withProgress(
    min = 0,
    max = nfiles,
    message = "reading session logs",
    for (i in seq_along(allfiles)) {
      out <- read.csv(allfiles[[i]][grep("0_log.csv", allfiles[[i]])])
      out <- out[, 1:4, drop = FALSE]
      # if (nrow(out) == 0) out[1, ] <- NA
      if (nrow(out) > 0) sessionlogs[[i]] <- cbind(fnames[i], out)
      pcounter <- pcounter + 1
      incProgress(pcounter)
      Sys.sleep(0.05)
    }
  )
  sessionlogs <- do.call("rbind", sessionlogs)
  
  # read focal data
  focalsessions <- list()
  pcounter <- 0
  withProgress(
    min = 0,
    max = sum(collectionlogs$focal_sessions_so_far),
    message = "reading focal sessions",
    for (i in seq_along(allfiles)) {
      a <- allfiles[[i]]
      
      a <- a[grep("_foctab.csv$", a)]
      if (length(a) > 0) {
        for (f in a) {
          out <- read.csv(f)
          out <- cbind(session_id = basename(gsub(pattern = "_foctab.csv$", "", f)), date = NA, out)
          out <- out[, !colnames(out) %in% c("sample", "time_stamp")]
          out$date <- as.Date(substr(out$session_id, 1, 10))
          out$scratches <- as.integer(out$scratches)
          focalsessions[[length(focalsessions) + 1]] <- out
          
          pcounter <- pcounter + 1
          incProgress(pcounter)
          Sys.sleep(0.05)
        }
      }
    }
  )
  focalsessions <- do.call("rbind", focalsessions)
  
  
  
  
  
  # update selections
  updateCheckboxGroupInput(inputId = "group_sel", selected = unique(collectionlogs$group))
  updateCheckboxGroupInput(inputId = "observer_sel", selected = unique(collectionlogs$observer))
  updateDateInput(inputId = "date_from", value = min(collectionlogs$date))
  updateDateInput(inputId = "date_to", value = max(collectionlogs$date))
  
  
  # make reactive objects
  xdata <- reactiveValues(collectionlogs = collectionlogs, trigger = runif(1))
  
  # select
  observeEvent(input$date_from, xdata$trigger <- runif(1))
  observeEvent(input$date_to, xdata$trigger <- runif(1))
  observeEvent(input$group_sel, xdata$trigger <- runif(1))
  observeEvent(input$observer_sel, xdata$trigger <- runif(1))
  
  observeEvent(xdata$trigger, {
    sel <- which(collectionlogs$date <= input$date_to & collectionlogs$date >= input$date_from & collectionlogs$group %in% input$group_sel & collectionlogs$observer %in% input$observer_sel)
    xdata$collectionlogs <- collectionlogs[sel, ]
  })
  
  output$collectionlog <- renderRHandsontable(rhandsontable(xdata$collectionlogs, rowHeaders = FALSE, readOnly = TRUE, digits = 1))
  
  
  
  
  output$focalsessions <- renderRHandsontable(rhandsontable(focalsessions, rowHeaders = FALSE, readOnly = TRUE))
  
  
  # output$sessionlog <- renderRHandsontable(rhandsontable(sessionlogs, rowHeaders = FALSE))
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
