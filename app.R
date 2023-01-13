# this is the final app...

library(shiny)
library(rhandsontable)


source("helpers/empty_foc_table.R")
source("helpers/focal_start_session_dialog.R")
source("helpers/focal_start_session.R")
source("helpers/adlib_aggression_dyadic_dialog.R")
source("helpers/empty_adlib_table.R")
source("helpers/add_one_minute.R")
source("helpers/focal_grooming_start_dialog.R")
source("helpers/focal_grooming_change_dialog.R")
source("helpers/empty_grooming.R")
source("helpers/focal_aggression_dialog.R")
source("helpers/render_nn.R")
source("helpers/empty_focal_aggression_table.R")


# individual table
all_individuals <- read.csv("id_table.csv", stringsAsFactors = FALSE)
all_individuals$present <- FALSE
all_individuals$swelling <- factor(NA, levels = 1:3)
all_individuals$comment <- ""
# list with observers (to be outsourced to csv file eventually)
all_observers <- c("maria", "carel", "jeanne", "robert", "joan", "julia")
# list with all activity codes for point sampling
activity_codes <- c("r", "fe", "gr", "oos")

# temporary placeholder for grooming partners
groompartners_temp <- LETTERS



ui <- fluidPage(
  tags$style(HTML(".bg-f { text-align: center; border-radius: 50px; background-color: rgba(255, 0, 0, 0.05); padding: 10px; color: black; font-weight: bolder; font-size: large; } # input[type='checkbox']{ width: 30px; height: 30px; line-height: 30px;}")),
  tags$style(HTML(".bg-fsel { text-align: center; border-radius: 50px; background-color: rgba(255, 0, 0, 0.6); padding: 10px; color: white; font-weight: bolder; font-size: large;}")),
  tags$style(HTML(".bg-m { text-align: center; border-radius: 50px; background-color: rgba(0, 0, 255, 0.05); padding: 10px; color: black; font-weight: bolder; font-size: large}")),
  tags$style(HTML(".bg-msel { text-align: center; border-radius: 50px; background-color: rgba(0, 0, 255, 0.6); padding: 10px; color: white; font-weight: bolder; font-size: large}")),
  tags$style(HTML(".bg-o { text-align: center; border-radius: 50px; background-color: rgba(10, 10, 10, 0.05); padding: 10px; color: black; font-weight: bolder; font-size: large}")),
  tags$style(HTML(".bg-osel { text-align: center; border-radius: 50px; background-color: rgba(10, 10, 10, 0.6); padding: 10px; color: white; font-weight: bolder; font-size: large}")),

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
  navbarPage("give me data collada", id = "nav_home",
             tabPanel("home",
                      column(2, "",
                             htmlOutput("dategroupobs")
                      ),
                      column(10, "",
                             # HTML('<p style= "color: red">bla</p>'),
                             actionButton(inputId = "start_focal_session_dialog_btn", label = "start focal session", style = "background: rgba(255, 0, 0, 0.5); height:100px", icon = icon("hourglass-start")),
                             actionButton(inputId = "go_to_census_btn", label = "go to census panel", style = "background: rgba(155, 0, 0, 0.5); height:50px"),
                             hr(),
                             h3("adlib events, IGE, etc"),
                             actionButton(inputId = "adlib_aggression_dialog", label = "dyadic aggression", style = "background: rgba(0, 0, 0, 0.5)")

                      )
             ),
             tabPanel("focal",
                      column(2, "",
                             textOutput("focal_dur_progress"),

                             actionButton("record_focal_groom_start_btn", "grooming start"),
                             actionButton("record_focal_groom_change_btn", "grooming switch/end"),
                             hr(),
                             actionButton("record_focal_aggr", "aggression (or some other event)"),
                             hr(),
                             actionButton("nn_scan", "nearest neighbour 'scan'"),
                             hr(),
                             actionButton("finish_focal_session", "finish session")
                      ),
                      column(10, "",
                             span(HTML("<p style='color:Khaki;'>the first two columns will eventually be hidden; other columns can be added as required; names can be changed</p>")),
                             conditionalPanel(condition = 'output.panelStatus', tagAppendAttributes(tag = textOutput("focal_grooming_in_progress"), class = 'blink_me')),
                             # conditionalPanel(condition = 'output.panelStatus', tagAppendAttributes(tag = textOutput("focal_grooming_in_progress"))),
                             rHandsontableOutput("focal_table")
                      )
             ),
             tabPanel("census",
                      rHandsontableOutput("census_table"),
                      actionButton("addnewrowtocensus", "add new row")
             ),

             tabPanel("nearest neighbors",
                      actionButton("submit_nn", "submit scan"),
                      p(),
                      fluidRow(htmlOutput("nn_fem")),
                      p(),
                      fluidRow(htmlOutput("nn_male")),
                      p(),
                      fluidRow(htmlOutput("nn_other"))
             ),

             tabPanel("review data",
                      span(HTML("<p style='color:Khaki;'>This particular tab here is still work in progress.</p>")),
                      selectInput("session_for_review", label = "select session", choices = c("one", "or", "the", "other")),
                      navlistPanel(widths = c(2, 10),
                                   tabPanel("focal (point samples)",
                                            rHandsontableOutput("rev_focal_table")),
                                   tabPanel("aggression",
                                            rHandsontableOutput("rev_aggression")),
                                   tabPanel("grooming",
                                            rHandsontableOutput("rev_groom")),
                                   tabPanel("neighbors",
                                            rHandsontableOutput("rev_nn"))
                      )
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
                      h4("grooming table:"),
                      tableOutput("debugging_groom"),
                      h4("current adlib aggression:"),
                      tableOutput("debug_adlib_aggression"),
                      h4("current focal table in static form"),
                      htmlOutput("debug_foctab_progress"),
                      tableOutput("static_foctab")
             )

  )
)


server <- function(input, output, session) {
  # create folder file storage
  if (!dir.exists("www")) dir.create("www")
  # get a conditional panel (grooming progress indicator) dependent on reactive values in the server
  output$panelStatus <- reactive({
    events_grooming$grooming_in_progress
  })
  outputOptions(output, "panelStatus", suspendWhenHidden = FALSE)

  # xdata: for daily info (group, observer, date)
  xdata <- reactiveValues(presence = all_individuals, get_started = FALSE)
  # v: for a single focal session
  v <- reactiveValues(foctab = NULL, # the actual data table
                      session_start = Sys.time(),
                      focal_id = NULL,
                      focal_session_identifier = NULL,
                      session_is_active = FALSE,
                      progress = NULL)
  # monitor sessions across day
  daily_sessions <- reactiveValues(sessions_over_day = matrix(ncol = 3, nrow = 0, dimnames = list(NULL,  c("session", "filename", "focal_id"))))
  # adlib aggression data
  adlib_agg <- reactiveValues(dyadic = empty_adlib_table())
  # focal session grooming
  events_grooming <- reactiveValues(grooming_in_progress = FALSE,
                                    grooming_direction = NA,
                                    current_grooming_parter = NA,
                                    withinsession_num = 1,
                                    withinevent_num = 1,
                                    grooming = empty_grooming())
  # focal session aggression
  focal_aggression_data <- reactiveValues(aggression = empty_focal_aggression_table())
  # nearest neighbors
  nn <- reactiveValues(ini_state = NULL,# setNames(rep(FALSE, n), ids)
                       firstrun = TRUE,
                       sex = NULL,
                       ids = NULL,
                       final = NULL)




  # reviewing of existing data ---------------------------
  output$rev_focal_table <- renderRHandsontable({
    sess <- gsub(".*\\((.*)\\).*", "\\1", input$session_for_review)
    if (nrow(daily_sessions$sessions_over_day) > 0 & file.exists(paste0("www/", sess, ".csv"))) {
      outtab <- read.csv(paste0("www/", sess, ".csv"))
      outtab <- rhandsontable(outtab, rowHeaders = NULL, height = 500)
      # outtab <- hot_col(outtab, "scratches", readOnly = TRUE)
      # hot_table(outtab, highlightCol = TRUE, highlightRow = TRUE)
      outtab <- hot_context_menu(outtab, allowRowEdit = FALSE, allowColEdit = FALSE)
      outtab
    }
  })
  output$rev_nn <- renderRHandsontable({
    sess <- gsub(".*\\((.*)\\).*", "\\1", input$session_for_review)
    p <- paste0("www/", sess, "-nn.csv")
    if (nrow(daily_sessions$sessions_over_day) > 0 & file.exists(p)) {
      if (!isTRUE(readLines(p) == "")) {
        outtab <- read.csv(p)
        colnames(outtab) <- c("id", paste0("scan", seq_len(ncol(outtab) - 1)))
        print(head(outtab))
        outtab <- rhandsontable(outtab, rowHeaders = NULL, height = 500)
        # outtab <- hot_col(outtab, "scratches", readOnly = TRUE)
        # hot_table(outtab, highlightCol = TRUE, highlightRow = TRUE)
        outtab <- hot_context_menu(outtab, allowRowEdit = FALSE, allowColEdit = FALSE)
        outtab
      }
    }
  })
  output$rev_groom <- renderRHandsontable({
    sess <- gsub(".*\\((.*)\\).*", "\\1", input$session_for_review)
    if (nrow(daily_sessions$sessions_over_day) > 0 & file.exists(paste0("www/", sess, "-groom.csv"))) {
      outtab <- read.csv(paste0("www/", sess, "-groom.csv"))[-1, ]
      # print(head(outtab))
      outtab <- rhandsontable(outtab, rowHeaders = NULL, height = 500)
      # outtab <- hot_col(outtab, "scratches", readOnly = TRUE)
      # hot_table(outtab, highlightCol = TRUE, highlightRow = TRUE)
      outtab <- hot_context_menu(outtab, allowRowEdit = FALSE, allowColEdit = FALSE)
      outtab
    }
  })
  output$rev_aggression <- renderRHandsontable({
    sess <- gsub(".*\\((.*)\\).*", "\\1", input$session_for_review)
    if (nrow(daily_sessions$sessions_over_day) > 0 & file.exists(paste0("www/", sess, "-aggr.csv"))) {
      outtab <- read.csv(paste0("www/", sess, "-aggr.csv"))
      outtab <- outtab[-nrow(outtab), ]
      # print(head(outtab))
      outtab <- rhandsontable(outtab, rowHeaders = NULL, height = 500)
      # outtab <- hot_col(outtab, "scratches", readOnly = TRUE)
      # hot_table(outtab, highlightCol = TRUE, highlightRow = TRUE)
      outtab <- hot_context_menu(outtab, allowRowEdit = FALSE, allowColEdit = FALSE)
      outtab
    }
  })






  # nearest neighbors -------------------------
  observeEvent(input$nn_scan, {
    if (v$session_is_active) {
      updateTabsetPanel(session, inputId = "nav_home", selected = "nearest neighbors")
      # print(nn_reactive())
    }
  })

  nn_reactive <- reactive({
    # ids <- c(all_individuals$id[all_individuals$group == input$group], c(paste0(c("AM"), 1:6), paste0(c("AF"), 1:6), paste0(c("J"), 1:6), paste0(c("I"), 1:6)))
    if (v$session_is_active) {
      o <- setNames(unlist(lapply(1:length(nn$ids), function(X) {
        input[[paste0("id_", nn$ids[X])]]
      })), nn$ids)
    }
  })

  observe({
    if (v$session_is_active) {
      lapply(isolate(nn$ids), function(X) {
        observeEvent(input[[paste0("id_", X)]], {
          if (X == nn$ids[length(nn$ids)]) {
            nn$firstrun <- FALSE
          }
          nn$ini_state <- nn_reactive()
        })
      })
    }
  })

  observeEvent(input$submit_nn, {
    # store to reactive object
    if (is.null(nn$final)) {
      nn_final <- cbind(nn$ids, nn_reactive())
    } else {
      nn_final <- cbind(nn$final, nn_reactive())
    }
    nn$final <- nn_final
    # reset for next scan:
    nn$ini_state <- setNames(rep(FALSE, length(nn$ids)), nn$ids)
    nn$firstrun <- TRUE

    updateTabsetPanel(session, inputId = "nav_home", selected = "focal") # shift focus to home tab
  })




  output$nn_fem <- renderUI({
    # ids <- c(all_individuals$id[all_individuals$group == input$group], c(paste0(c("AM"), 1:6), paste0(c("AF"), 1:6), paste0(c("J"), 1:6), paste0(c("I"), 1:6)))
    if (v$session_is_active) {
      if (nn$firstrun) {
        lapply(render_nn(nn$ids, selected = nn$ini_state, sex = nn$sex, do_which = "f"), function(X) HTML(paste(X)))
      } else {
        lapply(render_nn(nn$ids, selected = nn_reactive(), sex = nn$sex, do_which = "f"), function(X) HTML(paste(X)))
      }
    }
  })
  output$nn_male <- renderUI({
    ids <- c(all_individuals$id[all_individuals$group == input$group], c(paste0(c("AM"), 1:6), paste0(c("AF"), 1:6), paste0(c("J"), 1:6), paste0(c("I"), 1:6)))
    if (v$session_is_active) {
      if (nn$firstrun) {
        lapply(render_nn(ids, selected = nn$ini_state, sex = nn$sex, do_which = "m"), function(X) HTML(paste(X)))
      } else {
        lapply(render_nn(ids, selected = nn_reactive(), sex = nn$sex, do_which = "m"), function(X) HTML(paste(X)))
      }
    }
  })
  output$nn_other <- renderUI({
    ids <- c(all_individuals$id[all_individuals$group == input$group], c(paste0(c("AM"), 1:6), paste0(c("AF"), 1:6), paste0(c("J"), 1:6), paste0(c("I"), 1:6)))
    if (v$session_is_active) {
      if (nn$firstrun) {
        lapply(render_nn(ids, selected = nn$ini_state, sex = nn$sex, do_which = "o"), function(X) HTML(paste(X)))
      } else {
        lapply(render_nn(ids, selected = nn_reactive(), sex = nn$sex, do_which = "o"), function(X) HTML(paste(X)))
      }
    }
  })





  # grooming -----------------------
  observeEvent(input$record_focal_groom_start_btn, {
    if (events_grooming$grooming_in_progress == TRUE) {
      modalDialog()
    } else {
      if (events_grooming$grooming_in_progress == FALSE & v$session_is_active == TRUE) showModal(focal_grooming_start_dialog(focal_id = v$focal_id, partners = groompartners_temp))
    }

  })
  observeEvent(input$record_focal_groom_change_btn, {
    # print(events_grooming$grooming_in_progress == TRUE)
    if (events_grooming$grooming_in_progress == TRUE & v$session_is_active == TRUE) showModal(focal_grooming_change_dialog(events_grooming = events_grooming))
  })

  observeEvent(input$start_grooming, {
    events_grooming$grooming_direction <- input$grooming_focal_direction
    events_grooming$current_grooming_parter <- input$grooming_partner_name
    events_grooming$grooming <- rbind(events_grooming$grooming, NA)
    events_grooming$grooming$withinevent_num[nrow(events_grooming$grooming)] <- events_grooming$withinevent_num
    events_grooming$grooming$time_stamp[nrow(events_grooming$grooming)] <- Sys.time()
    events_grooming$grooming$session[nrow(events_grooming$grooming)] <- v$focal_session_identifier
    events_grooming$grooming$focal[nrow(events_grooming$grooming)] <- v$focal_id
    events_grooming$grooming$partner[nrow(events_grooming$grooming)] <- events_grooming$current_grooming_parter
    events_grooming$grooming$withinsession_num[nrow(events_grooming$grooming)] <- events_grooming$withinsession_num
    events_grooming$grooming$direction[nrow(events_grooming$grooming)] <- events_grooming$grooming_direction
    events_grooming$grooming$approach_by_focal[nrow(events_grooming$grooming)] <- input$grooming_focal_approach
    events_grooming$grooming$initated_by_focal[nrow(events_grooming$grooming)] <- input$grooming_focal_init

    events_grooming$grooming_in_progress <- TRUE
    removeModal()
    output$debugging_groom <- renderTable(events_grooming$grooming[-1, ])
    events_grooming$withinevent_num <- events_grooming$withinevent_num  + 1

    # progress update
    if (events_grooming$grooming_direction == "gives") {
      output$focal_grooming_in_progress <- renderText(c("grooming is in progress: ", v$focal_id, "grooms", events_grooming$current_grooming_parter))
    }
    if (events_grooming$grooming_direction == "receives") {
      output$focal_grooming_in_progress <- renderText(c("grooming is in progress: ", events_grooming$current_grooming_parter, "grooms", v$focal_id))
    }
    if (events_grooming$grooming_direction == "mutual") {
      output$focal_grooming_in_progress <- renderText(c("grooming is in progress: ", v$focal_id, "and", events_grooming$current_grooming_parter, "groom each other"))
    }
  })

  observeEvent(input$change_grooming, {
    events_grooming$grooming_direction <- input$grooming_focal_direction_change
    events_grooming$grooming <- rbind(events_grooming$grooming, NA)
    events_grooming$grooming$withinevent_num[nrow(events_grooming$grooming)] <- events_grooming$withinevent_num
    events_grooming$grooming$time_stamp[nrow(events_grooming$grooming)] <- Sys.time()
    events_grooming$grooming$session[nrow(events_grooming$grooming)] <- v$focal_session_identifier
    events_grooming$grooming$focal[nrow(events_grooming$grooming)] <- v$focal_id
    events_grooming$grooming$partner[nrow(events_grooming$grooming)] <- events_grooming$current_grooming_parter
    events_grooming$grooming$withinsession_num[nrow(events_grooming$grooming)] <- events_grooming$withinsession_num
    events_grooming$grooming$direction[nrow(events_grooming$grooming)] <- events_grooming$grooming_direction
    events_grooming$grooming$approach_by_focal[nrow(events_grooming$grooming)] <- input$grooming_focal_approach
    events_grooming$grooming$initated_by_focal[nrow(events_grooming$grooming)] <- input$grooming_focal_init

    removeModal()
    events_grooming$withinevent_num <- events_grooming$withinevent_num  + 1
    output$debugging_groom <- renderTable(events_grooming$grooming[-1, ])

    if (events_grooming$grooming_direction == "gives") {
      output$focal_grooming_in_progress <- renderText(c("grooming is in progress: ", v$focal_id, "grooms", events_grooming$current_grooming_parter))
    }
    if (events_grooming$grooming_direction == "receives") {
      output$focal_grooming_in_progress <- renderText(c("grooming is in progress: ", events_grooming$current_grooming_parter, "grooms", v$focal_id))
    }
    if (events_grooming$grooming_direction == "mutual") {
      output$focal_grooming_in_progress <- renderText(c("grooming is in progress: ", v$focal_id, "and", events_grooming$current_grooming_parter, "groom each other"))
    }
  })
  observeEvent(input$stop_grooming, {
    events_grooming$grooming <- rbind(events_grooming$grooming, NA)
    events_grooming$grooming$withinevent_num[nrow(events_grooming$grooming)] <- events_grooming$withinevent_num
    events_grooming$grooming$time_stamp[nrow(events_grooming$grooming)] <- Sys.time()
    events_grooming$grooming$session[nrow(events_grooming$grooming)] <- v$focal_session_identifier
    events_grooming$grooming$focal[nrow(events_grooming$grooming)] <- v$focal_id
    events_grooming$grooming$partner[nrow(events_grooming$grooming)] <- events_grooming$current_grooming_parter
    events_grooming$grooming$withinsession_num[nrow(events_grooming$grooming)] <- events_grooming$withinsession_num
    events_grooming$grooming$direction[nrow(events_grooming$grooming)] <- "end"
    events_grooming$grooming$approach_by_focal[nrow(events_grooming$grooming)] <- input$grooming_focal_approach
    events_grooming$grooming$initated_by_focal[nrow(events_grooming$grooming)] <- input$grooming_focal_init
    events_grooming$grooming$leave_by[events_grooming$grooming$withinsession_num == events_grooming$withinsession_num] <- input$grooming_focal_leave

    removeModal()
    events_grooming$grooming_in_progress <- FALSE
    events_grooming$grooming_direction <- NA
    events_grooming$current_grooming_parter <- NA
    events_grooming$withinsession_num <- events_grooming$withinsession_num + 1
    events_grooming$withinevent_num <- 1
    output$debugging_groom <- renderTable(events_grooming$grooming[-1, ])
  })

  # other focal session aggression -------------------
  observeEvent(input$record_focal_aggr, {
    if (v$session_is_active) {
      showModal(focal_aggression_dialog(focal_id = v$focal_id)) # submit button in dialog: 'focal_aggression'
    }
  })
  observeEvent(input$focal_aggression, {
    if (v$session_is_active) {
      focal_aggression_data$aggression <- rbind(NA, focal_aggression_data$aggression)
      focal_aggression_data$aggression$time_stamp[1] <- input$focal_aggression_dyadic_datetime
      focal_aggression_data$aggression$focal[1] <- input$focal_aggression_dyadic_id1
      focal_aggression_data$aggression$id2[1] <- input$focal_aggression_dyadic_id2
      focal_aggression_data$aggression$highest_intensity[1] <- input$focal_aggression_dyadic_intensity
      focal_aggression_data$aggression$focal_won[1] <- input$focal_aggression_dyadic_focal_won
      removeModal()
    }
  })


  # start session ----------------------------
  observeEvent(input$start_focal_session_dialog_btn, {
    # check whether a session is already running
    # print(paste("session is active:", v$session_is_active, "\n"))
    # print(paste("foctab is NULL:", is.null(v$foc_tab), "\n"))
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
    eft <- empty_foc_table(start_time = strptime(input$focal_start, format = "%Y-%m-%d %H:%M:%S"),
                           duration = input$focal_duration, id = input$focal_name, activity_codes = activity_codes)
    v$foctab <- eft
    v$focal_id <- input$focal_name
    v$progress <- list(target = input$focal_duration, table_lines = input$focal_duration, na_vals = input$focal_duration, oos = 0, act = 0)
    v$session_is_active <- TRUE
    s <- paste(sample(c(letters, 0:9), 8, replace = TRUE), collapse = "")
    v$focal_session_identifier <- s
    # v$filename <- file.path(tempdir(), paste0(s, ".csv"))
    v$filename <- file.path("www", paste0(s, ".csv"))
    write.table(v$foctab, file = v$filename, sep = ",", row.names = FALSE, quote = FALSE, dec = ".")
    # print(v$filename)
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

    # reset grooming
    events_grooming$grooming_in_progress = FALSE
    events_grooming$grooming_direction = NA
    events_grooming$current_grooming_parter = NA
    events_grooming$withinsession_num = 1
    events_grooming$withinevent_num = 1
    events_grooming$grooming = empty_grooming()

    # reset focal aggression
    focal_aggression_data$aggression <- empty_focal_aggression_table()


    # set nn data
    # ids <- c(all_individuals$id[all_individuals$group == input$group], c(paste0(c("AM"), 1:6), paste0(c("AF"), 1:6), paste0(c("J"), 1:6), paste0(c("I"), 1:6)))
    nn$ids <- c(all_individuals$id[all_individuals$group == input$group], c(paste0(c("AM"), 1:6), paste0(c("AF"), 1:6), paste0(c("J"), 1:6), paste0(c("I"), 1:6)))
    nn$ini_state <- setNames(rep(FALSE, length(nn$ids)), nn$ids)
    nn$sex <- c(all_individuals$sex[all_individuals$group == input$group], rep("o", 24))
    nn$final <- NULL
  })

  remcols <- c("time_stamp", "sample")
  output$focal_table <- renderRHandsontable({
    if (!is.null(v$foctab)) {
      # outtab <- v$foctab[, -c(which(colnames(v$foctab) %in% remcols))]
      outtab <- v$foctab
      outtab <- rhandsontable(outtab, rowHeaders = NULL, height = 500)
      # outtab <- hot_col(outtab, "scratches", readOnly = TRUE)
      # hot_table(outtab, highlightCol = TRUE, highlightRow = TRUE)
      outtab <- hot_context_menu(outtab, allowRowEdit = FALSE, allowColEdit = FALSE)
      outtab
    }
  })
  observeEvent(input$focal_table, {
    xxx <- hot_to_r(input$focal_table)
    write.table(xxx, file = v$filename, sep = ",", row.names = FALSE, quote = FALSE, dec = ".")
    output$static_foctab <- renderTable(xxx)


    # update progress tracker
    # add rows if required
    # automated better than manually via addrow-button
    v$progress$oos <- sum(xxx$activity %in% "oos")
    v$progress$act <- sum(xxx$activity %in% activity_codes) - v$progress$oos
    v$progress$na_vals <- sum(is.na(xxx$activity))
    v$progress$table_lines <- nrow(xxx)
    if (v$progress$act < v$progress$target & !is.na(xxx$activity[nrow(xxx)])) {
      xxx <- rbind(xxx, empty_foc_table(start_time = Sys.time(), duration = 1, id = input$focal_name, activity_codes = activity_codes))
      xxx$sample[nrow(xxx)] <- nrow(xxx)
      xxx$time_for_display[nrow(xxx)] <- add_one_minute(xxx$time_for_display[nrow(xxx) - 1])
      v$progress$table_lines <- nrow(xxx) + 1
      Sys.sleep(1)
    }

    # browser()
    v$foctab <- xxx
    output$debug_foctab_progress <- renderUI({
      HTML(paste("activity samples:", v$progress$act, "<br>", "oos samples:", v$progress$oos, "<br>",
                 "NA samples:", v$progress$na_vals, "<br>", "target:", v$progress$target, "<br>", "num rows in table:", v$progress$table_lines))
    })
  })
  # progress tracker for session
  observeEvent(v$progress$act, {
    if (v$progress$act > 0) {
      output$focal_dur_progress <- renderText(paste(v$progress$act, "of", input$focal_duration, "done"))
    } else {
      output$focal_dur_progress <- NULL
    }
  })

  # end session -------------------------
  observeEvent(input$finish_focal_session, {
    if (v$session_is_active) {
      temp_object <- v$foctab
      temp_object$time_stamp <- as.character(temp_object$time_stamp)
      # store focal table
      write.csv(temp_object, file = v$filename, row.names = FALSE, quote = FALSE)
      # store nn object
      write.csv(nn$final, file = paste0("www/", v$focal_session_identifier, "-nn.csv"), row.names = FALSE, quote = FALSE)
      # store grooming
      write.csv(events_grooming$grooming, file = paste0("www/", v$focal_session_identifier, "-groom.csv"), row.names = FALSE, quote = FALSE)
      # store aggression
      write.csv(focal_aggression_data$aggression, file = paste0("www/", v$focal_session_identifier, "-aggr.csv"), row.names = FALSE, quote = FALSE)

      # update list for revisions
      session_id_for_display <- paste0(daily_sessions$sessions_over_day[, "focal_id"], " (", as.character(daily_sessions$sessions_over_day[, "session"]), ")")
      updateSelectInput(inputId = "session_for_review", choices = session_id_for_display)

      # reset reactive values object
      v$foctab = NULL # the actual data table
      v$session_start = Sys.time()
      v$focal_id = NULL
      v$focal_session_identifier = NULL
      v$session_is_active = FALSE
      v$progress <- NULL
      updateTabsetPanel(session, inputId = "nav_home", selected = "home") # shift focus to home tab
    }
  })





  # start up message and setup -----------------------
  showModal(modalDialog(title = "hello there, what's up today?",
                        span("please provide the necessary information"), hr(),
    dateInput("date", "date"),
    selectInput("observer", "observer", choices = unique(sample(all_observers))),
    # selectInput("group", "group", choices = c(all_individuals$group)),
    selectInput("group", "group", choices = unique(all_individuals$group), selected = "pb"),
    footer = tagList(
      # modalButton("Cancel"),
      actionButton("startnewday_ok", "OK", style = "background: rgba(0, 255, 0, 0.5); height:100px; width:100px"),
      HTML("<p style='color:Khaki;'>to be done: 'are you sure?'-button")
    )
  ))
  observeEvent(input$startnewday_ok, {
    xdata$presence <- xdata$presence[xdata$presence$group == input$group, ] # select relevant group...
    xdata$get_started <- TRUE
    output$dategroupobs <- renderText({
      paste("<p>selected group:", "<b style='color:red'>", input$group, "</b></p>", "<hr>",
            "<p>selected date:<b>", as.character(input$date), "</b></p>", "<hr>",
            "<p>selected observer:<b>", as.character(input$observer), "</b></p>")
    })
    removeModal()
  })

  observeEvent(input$census_table, {
    # print(paste("presence is NULL:", is.null(xdata$presence), "\n"))
    # print(paste("day got started:", xdata$get_started, "\n"))
    xxx <- hot_to_r(input$census_table)
    # print(paste("nrow presence:", nrow(xxx), "\n"))
    if (!is.null(xdata$presence) & xdata$get_started == TRUE) {
      xxx <- hot_to_r(input$census_table)
      write.table(xxx, file = "www/census.csv", sep = ",", row.names = FALSE, quote = FALSE, dec = ".")
    }
  })
  observeEvent(input$addnewrowtocensus, {
    if (!is.null(xdata$presence) & xdata$get_started == TRUE) {
      xxx <- hot_to_r(input$census_table)
      xxx <- rbind(xxx, NA)
      xxx$present[nrow(xxx)] <- FALSE
      xxx$group[nrow(xxx)] <- xxx$group[1]
      xdata$presence <- xxx
    }
  })


  output$census_table <- renderRHandsontable({
    if (!is.null(xdata$presence) & xdata$get_started == TRUE) {
      xtab <- rhandsontable(xdata$presence, rowHeaders = NULL)
      # xtab <- rhandsontable(xdata$presence[, -c(which(colnames(xdata$presence) %in% c("is_focal", "sex")))], rowHeaders = NULL)
      # xtab <- hot_row(xtab, c(1,3, 5), readOnly = TRUE)
      # xtab <- hot_col(xtab, c(1), readOnly = TRUE)
      # xtab <- hot_col(xtab, c(1), readOnly = TRUE)
      hot_table(xtab, highlightCol = TRUE, highlightRow = TRUE)
    }
  })

  observeEvent(input$adlib_aggression_dialog, {
    showModal(adlib_aggression_dyadic_dialog()) # submit button in dialog: 'adlib_aggression'
  })
  observeEvent(input$adlib_aggression, {
    adlib_agg$dyadic <- rbind(NA, adlib_agg$dyadic)
    adlib_agg$dyadic$time_stamp[1] <- input$adlib_aggression_dyadic_datetime
    adlib_agg$dyadic$id1[1] <- input$adlib_aggression_dyadic_id1
    adlib_agg$dyadic$id2[1] <- input$adlib_aggression_dyadic_id2
    adlib_agg$dyadic$highest_intensity[1] <- input$adlib_aggression_dyadic_intensity
    removeModal()
  })

  observeEvent(input$go_to_census_btn, {
    updateTabsetPanel(session, inputId = "nav_home", selected = "census") # shift focus to census tab
  })



  # simple debuggging/diagnostics elements
  output$debug_adlib_aggression <- renderTable(adlib_agg$dyadic)
  output$rsession_info <- renderPrint(sessionInfo())
  output$current_wd <- renderPrint(getwd())
}

# Run the application
shinyApp(ui = ui, server = server)
