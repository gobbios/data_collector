# this is the final app...

library(shiny)
library(rhandsontable)


source("helpers/empty_foc_table.R")
source("helpers/focal_start_session_dialog.R")
source("helpers/focal_start_session.R")


# individual table 
all_individuals <- read.csv("id_table.csv", stringsAsFactors = FALSE)
all_individuals$present <- FALSE
all_individuals$swelling <- factor(NA, levels = 1:3)
all_individuals$comment <- ""
# list with observers (to be outsourced to csv file eventually)
all_observers <- c("maria", "carel", "jeanne")
# list with all activity codes for point sampling
activity_codes <- c("r", "fe", "gr")



ui <- fluidPage(
  navbarPage("give me data collada", id = "nav_home",
             tabPanel("home",
                      column(2, "",
                             htmlOutput("dategroupobs")
                      ),
                      column(10, "",
                             actionButton(inputId = "start_focal_session_dialog_btn", label = "start focal session", style = "background: rgba(255, 0, 0, 0.5); height:100px"),
                             actionButton(inputId = "go_to_census_btn", label = "go to census panel", style = "background: rgba(155, 0, 0, 0.5); height:50px")
                      )
             ),
             tabPanel("focal",
                      column(2, "",
                             
                             # actionButton("record_focal_aggr", "aggression"),
                             # actionButton("record_focal_groom_start", "grooming start"),
                             # actionButton("record_focal_groom_change", "grooming switch/end"),
                             actionButton("addnewrowtofoctab", "add new row"),
                             actionButton("finish_focal_session", "finish session")
                      ),
                      column(10, "",
                             # conditionalPanel(condition = 'output.panelStatus', tagAppendAttributes(tag = textOutput("focal_groming_in_progress"), class = 'blink_me')),
                             conditionalPanel(condition = 'output.panelStatus', tagAppendAttributes(tag = textOutput("focal_groming_in_progress"))),
                             rHandsontableOutput("focal_table")
                      )
             ),
             tabPanel("census",
                      rHandsontableOutput("census_table"),
                      actionButton("addnewrowtocensus", "add new row")
             ),
             tabPanel("diagnostics",
                      h4("focal session overview:"),
                      tableOutput("filenames_used"),
                      h4("links to the generated csv files (per focal session):"),
                      uiOutput("filenames_links"),
                      h4("current working directory"),
                      verbatimTextOutput("current_wd"),
                      h4("R session info"),
                      verbatimTextOutput("rsession_info")
             ),
             tabPanel("debugging",
                      h4("current focal table in static form"),
                      tableOutput("static_foctab")
             )
             
  )
)


server <- function(input, output, session) {
  # create folder file storage
  if (!dir.exists("www")) dir.create("www")
  
  # xdata: for daily info (group, observer, date)
  xdata <- reactiveValues(presence = all_individuals, get_started = FALSE)
  # v: for a single focal session 
  v <- reactiveValues(foctab = NULL, # the actual data table
                      session_start = Sys.time(), 
                      focal_id = NULL,
                      focal_session_identifier = NULL,
                      session_is_active = FALSE)
  # monitor sessions across day
  daily_sessions <- reactiveValues(sessions_over_day =matrix(ncol = 3, nrow = 0, dimnames = list(NULL,  c("session", "filename", "focal_id"))))

 
  
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
      updateTabsetPanel(session, inputId = "nav_home", selected = "focal")
    }
  })
  
  observeEvent(input$focal_session_start, {
    # reset
    # events_grooming$grooming_in_progress <- FALSE
    eft <- empty_foc_table(start_time = strptime(input$focal_start, format = "%Y-%m-%d %H:%M:%S"), duration = 7, id = input$focal_name, activity_codes = activity_codes)
    v$foctab <- eft
    v$focal_id <- input$focal_name
    v$session_is_active <- TRUE
    s <- paste(sample(c(letters, 0:9), 8, replace = TRUE), collapse = "")
    v$focal_session_identifier <- s
    # v$filename <- file.path(tempdir(), paste0(s, ".csv"))
    v$filename <- file.path("www", paste0(s, ".csv"))
    write.table(v$foctab, file = v$filename, sep = ",", row.names = FALSE, quote = FALSE, dec = ".")
    print(v$filename)
    # update daily monitor
    daily_sessions$sessions_over_day <- rbind(NA, daily_sessions$sessions_over_day)
    daily_sessions$sessions_over_day[1, "session"] <- v$focal_session_identifier
    daily_sessions$sessions_over_day[1, "filename"] <- v$filename
    daily_sessions$sessions_over_day[1, "focal_id"] <- v$focal_id
    
    output$filenames_used <- renderTable({daily_sessions$sessions_over_day})
    output$filenames_links <- renderText({
      sapply(daily_sessions$sessions_over_day[, "session", drop = TRUE], function(y)HTML(paste(a(y, href = paste0(y, ".csv")))))
    })
    removeModal()
  })
  
  remcols <- c("time_stamp", "sample")
  output$focal_table <- renderRHandsontable({
    if (!is.null(v$foctab)) {
      outtab <- v$foctab[, -c(which(colnames(v$foctab) %in% remcols))]
      outtab <- rhandsontable(outtab, rowHeaders = NULL, height = 500)
      # outtab <- hot_col(outtab, "scratches", readOnly = TRUE)
      # hot_table(outtab, highlightCol = TRUE, highlightRow = TRUE)
      outtab <- hot_context_menu(outtab, allowRowEdit = FALSE, allowColEdit = FALSE)
      outtab
    }
  })
  observeEvent(input$focal_table, {
    xxx <- hot_to_r(input$focal_table)
    write.table(data.frame(time_stamp = v$foctab$time_stamp, xxx), file = v$filename, sep = ",", row.names = FALSE, quote = FALSE, dec = ".")
    output$static_foctab <- renderTable(xxx)
  })     
  observeEvent(input$addnewrowtofoctab, {
    if (!is.null(v$foctab) & v$session_is_active == TRUE) {
      v$foctab <- rbind(v$foctab, NA)
    }
  })
  observeEvent(input$finish_focal_session, {
    temp_object <- v$foctab
    temp_object$time_stamp <- as.character(temp_object$time_stamp)
    write.csv(temp_object, file = v$filename, row.names = FALSE, quote = FALSE)
    system2(command = "open", shQuote(v$filename))
    # reset reactive values object
    v$foctab = NULL # the actual data table
    v$session_start = Sys.time()
    v$focal_id = NULL
    v$focal_session_identifier = NULL
    v$session_is_active = FALSE
    
  })
  
  
  
  
  
  
  showModal(modalDialog(title = "hello there, what's up today?",
                        span("please provide the necessary information"), hr(),
    dateInput("date", "date"),
    selectInput("observer", "observer", choices = unique(all_observers)),
    selectInput("group", "group", choices = c(all_individuals$group)),
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
    hot = isolate(input$census_table)
    if (!is.null(hot)) {
      if (nrow(hot_to_r(input$census_table)) > 0) {
        xdata$presence$present <- hot_to_r(input$census_table)$present
      }
    }
  })
  observeEvent(input$addnewrowtocensus, {
    if (!is.null(xdata$presence) & xdata$get_started == TRUE) {
      xdata$presence <- rbind(xdata$presence, NA)
      xdata$presence$present[nrow(xdata$presence)] <- FALSE
      xdata$presence$group[nrow(xdata$presence)] <- xdata$presence$group[1]
    }
  })
  output$census_table = renderRHandsontable({
    if (!is.null(xdata$presence) & xdata$get_started == TRUE) {
      xtab <- rhandsontable(xdata$presence[, -c(which(colnames(xdata$presence) %in% c("is_focal", "sex")))], rowHeaders = NULL)
      # xtab <- hot_row(xtab, c(1,3, 5), readOnly = TRUE)
      # xtab <- hot_col(xtab, c(1), readOnly = TRUE)
      # xtab <- hot_col(xtab, c(1), readOnly = TRUE)
      hot_table(xtab, highlightCol = TRUE, highlightRow = TRUE)
    }
  })
  
  
  observeEvent(input$go_to_census_btn, {
    updateTabsetPanel(session, inputId = "nav_home", selected = "census")
  })
  
  output$rsession_info <- renderPrint(sessionInfo())
  output$current_wd <- renderPrint(getwd())
}

# Run the application
shinyApp(ui = ui, server = server)
