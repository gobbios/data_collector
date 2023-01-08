# a focal table for point samples
# uses dynamic/editable rhandsontable
# stores it dynamically (after any change to its content) as csv
# rereads from csv for 'dynamic' display

library(shiny)
library(rhandsontable)

source("../helpers/empty_foc_table.R")
source("../helpers/focal_start_session_dialog.R")
source("../helpers/focal_start_session.R")

all_individuals <- read.csv("../id_table.csv", stringsAsFactors = FALSE)
activity_codes <- c("r", "fe", "gr")

ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
          dateInput("date", "date"),
          textInput("observer", "observer", placeholder = "2 letter code"),
          selectInput("group", "group", choices = c(all_individuals$group)),
          actionButton(inputId = "start_focal_session_dialog_btn", label = "start focal session", style = "background: rgba(255, 0, 0, 0.5); height:100px")
          
        ),

        mainPanel(
          rHandsontableOutput("focal_table"),
          span("just a static version:"),
          tableOutput("focal_table_static"),
          span("just a dynamic version (reread from csv):"),
          tableOutput("focal_table_dynamic")
        )
    )
)

server <- function(input, output, session) {
    v <- reactiveValues(foctab = NULL,
                        session_start = Sys.time(),
                        focal_id = NULL,
                        focal_session_identifier = NULL,
                        session_is_active = FALSE)

    observeEvent(input$focal_session_start, {
      # reset
      # events_grooming$grooming_in_progress <- FALSE
      eft <- empty_foc_table(start_time = strptime(input$focal_start, format = "%Y-%m-%d %H:%M:%S"), duration = 7, id = input$focal_name, activity_codes = activity_codes)
      v$foctab <- eft
      v$focal_id <- input$focal_name
      v$session_is_active <- TRUE
      s <- paste(sample(c(letters, 0:9), 8, replace = TRUE), collapse = "")
      v$focal_session_identifier <- s
      v$filename <- file.path(tempdir(), paste0(s, ".csv"))
      write.table(v$foctab, file = v$filename, sep = ",", row.names = FALSE, quote = FALSE, dec = ".")
      print(v$filename)
      removeModal()
    })
    

    observeEvent(input$start_focal_session_dialog_btn, {
      # check whether a session is already running
      print(paste("session is active:", v$session_is_active, "\n"))
      print(paste("foctab is NULL:", is.null(v$foc_tab), "\n"))
      if (v$session_is_active ) {
        showModal(modalDialog(
          span("Active session detected. You can't have two sessions at the same time. Save the running session first before starting a new one."),
          footer = tagList(modalButton("Cancel"))
        ))
      } else {
        showModal(focal_start_session_dialog(potential_focals = all_individuals$id[all_individuals$group == input$group]))
        # updateTabsetPanel(session, inputId = "nav_home", selected = "focal")
      }
    })
    
    remcols <- c("time_stamp", "sample")
    output$focal_table <- renderRHandsontable({
      if (!is.null(v$foctab)) {
        outtab <- v$foctab[, -c(which(colnames(v$foctab) %in% remcols))]
        outtab <- rhandsontable(outtab, rowHeaders = NULL)
        # outtab <- hot_col(outtab, "scratches", readOnly = TRUE)
        # hot_table(outtab, highlightCol = TRUE, highlightRow = TRUE)
        outtab
      }
    })
    observeEvent(input$focal_table, {
      xxx <- hot_to_r(input$focal_table)
      write.table(data.frame(time_stamp = v$foctab$time_stamp, xxx), file = v$filename, sep = ",", row.names = FALSE, quote = FALSE, dec = ".")
    })      

    output$focal_table_static <- renderTable({
      outtab <- v$foctab[, -c(which(colnames(v$foctab) %in% remcols))]
      outtab
    })
    observeEvent(input$focal_table, {
      if (file.exists(v$filename)) {
        output$focal_table_dynamic <- renderTable({
          outtab <- read.csv(v$filename, sep = ",", dec = ".")
          outtab
        })
      }
    })
    # would require recoding of factors and other display issues because initially most columns are simply filled with NA's
    # observeEvent(input$focal_table, {
    #   if (file.exists(v$filename)) {
    #     v$foctab <- read.csv(v$filename, sep = ",", dec = ".")
    #   }
    # })

    
}

shinyApp(ui = ui, server = server)
