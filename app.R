# this is the final app...

library(shiny)
library(rhandsontable)

source("helpers/make_empty_objects.R")
source("helpers/html_styles.R")
source("helpers/review_tables.R")
source("helpers/startup_dialog_box.R")
source("helpers/link_directory.R")
source("helpers/focal_aggression.R")
source("helpers/focal_grooming.R")

source("helpers/empty_foc_table.R")
source("helpers/reload_sessions.R")
source("helpers/focal_start_session_dialog.R")
source("helpers/focal_start_session.R")
source("helpers/adlib_aggression_dyadic.R")
source("helpers/add_one_minute.R")
source("helpers/focal_grooming_start_dialog.R")
source("helpers/focal_grooming_change_dialog.R")
source("helpers/render_nn.R")
source("helpers/additional_group_for_census.R")

# individual table
all_individuals <- read.csv("id_table.csv", stringsAsFactors = FALSE)
all_individuals$present <- FALSE
all_individuals$swelling <- factor(NA, levels = c("", 0, 1, 2, 3))
all_individuals$comment <- ""
# add two new males for potential immigrants
for (i in unique(all_individuals$group)) {
  all_individuals <- rbind(NA, NA, all_individuals)
  all_individuals$id[1:2] <- c("new male 1", "new male 2")
  all_individuals$sex[1:2] <- "m"
  all_individuals$group[1:2] <- i
  all_individuals$is_focal[1:2] <- "no"
  all_individuals$present[1:2] <- FALSE
  all_individuals$swelling[1:2] <- NA
  all_individuals$comment[1:2] <- ""
}




# list with observers (to be outsourced to csv file eventually)
all_observers <- c("maria", "carel", "jeanne", "robert", "joan", "julia")
# list with all activity codes for point sampling
activity_codes <- c("r", "fe", "gr", "oos")

# temporary placeholder for grooming partners
groompartners_temp <- LETTERS



ui <- fluidPage(
  tags$style(html_boxes(stylename = "bg-f")), tags$style(html_boxes(stylename = "bg-fsel")), # styles for nn-checkboxes
  tags$style(html_boxes(stylename = "bg-m")), tags$style(html_boxes(stylename = "bg-msel")),
  tags$style(html_boxes(stylename = "bg-o")), tags$style(html_boxes(stylename = "bg-osel")),

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
                             htmlOutput("dategroupobs"),
                             actionButton("open_data_dir_abtn", "open data directory")
                      ),
                      column(10, "",
                             # HTML('<p style= "color: red">bla</p>'),
                             actionButton(inputId = "start_focal_session_dialog_abtn", label = "start focal session", style = "background: rgba(255, 0, 0, 0.5); height:100px", icon = icon("hourglass-start")),
                             actionButton(inputId = "go_to_census_btn", label = "go to census panel", style = "background: rgba(155, 0, 0, 0.5); height:50px"),
                             hr(),
                             h3("adlib events, IGE, etc"),
                             actionButton(inputId = "adlib_aggression_dialog", label = "dyadic aggression", style = "background: rgba(0, 0, 0, 0.5)"),
                             actionButton("test_stuff", "test stuff (print to console for debugging)")

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
                      actionButton("addnewrowtocensus", "add new row"),
                      actionButton("addgrouptocensus", "additional group")
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
                      h4("file paths to daily files:"),
                      verbatimTextOutput("info_paths_day"),
                      h4("focal session overview:"),
                      tableOutput("log"),
                      h4("current working directory"),
                      verbatimTextOutput("current_wd"),
                      h4("current content of meta data"),
                      verbatimTextOutput("metadata"),
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
             ),
             tabPanel("reload",
                      selectInput("available_days_selector", label = "select day and observer", choices = NULL),
                      actionButton("show_available_days", "show available days"),
                      actionButton("reload_selected_day", "reload selected day"),
                      navlistPanel(widths = c(2, 10),
                                   tabPanel("focal (point samples)",
                                            tableOutput("reload_days_available"),
                                            tableOutput("reload_sessions_available")
                                            )
                      )
             )

  )
)


server <- function(input, output, session) {
  # get a conditional panel (grooming progress indicator) dependent on reactive values in the server
  output$panelStatus <- reactive({
    metadata$grooming_in_progress
  })
  outputOptions(output, "panelStatus", suspendWhenHidden = FALSE)

  observeEvent(input$test_stuff, {
    print(unlist(isolate(reactiveValuesToList(v))$progress))
    print(unlist(isolate(reactiveValuesToList(metadata))))
    # print(isolate(reactiveValuesToList(fps)))
    # print(isolate(reactiveValuesToList(paths_sessions)))
  })

  # metadata (info regarding day): list with names
  # once per day: group date observer focal_sessions_so_far focal_duration
  # per focal session: focal_start focal_name get_started session_is_active current_foc_session_id
  # progress tracker within a focal session:
  # tracker for grooming:
  metadata <- empty_metadata()

  # xdata: for daily info (group, observer, date)
  xdata <- reactiveValues(presence = all_individuals)
  # v: for a single focal session
  v <- reactiveValues(foctab = NULL) # the actual data table

  # monitor sessions across day
  sessions_log <- reactiveValues(log = empty_log())
  # file paths
  # fixed throughout the day (logs, census etc)
  paths_day <- reactiveValues(data_root_dir = NULL, dirpath = NULL, daily_census = NULL, sessions_log = NULL, adlib_aggr = NULL, day_meta = NULL)
  # changing ones (focal sessions)
  paths_sessions <- reactiveValues(current_foc_tab = NULL, current_foc_nn = NULL, current_foc_groom = NULL, current_foc_aggr = NULL)

  # adlib aggression data
  adlib_agg <- reactiveValues(dyadic = empty_adlib_aggr())
  # focal session grooming
  grooming <- reactiveValues(grooming = empty_grooming())
  # focal session aggression
  focal_aggression_data <- reactiveValues(aggression = empty_focal_aggr())
  # nearest neighbors
  nn <- reactiveValues(ini_state = NULL, # ini_state = setNames(rep(FALSE, n), ids)
                       firstrun = TRUE,
                       sex = NULL,
                       ids = NULL,
                       final = NULL)
  individuals <- reactiveValues(for_nn = NULL)


  # reload data -------------------------
  observeEvent(input$show_available_days, {
    # available days
    ad <- reload_list_days(paths_day$data_root_dir)
    updateSelectInput(session, inputId = "available_days_selector", choices = ad$day_folder_display)
    output$reload_days_available <- renderTable(ad)
  })
  observeEvent(input$reload_selected_day, {
    # print(input$available_days_selector)
    # print(paths_day$data_root_dir)
    if (!is.null(input$available_days_selector) & input$available_days_selector != "") {
      ss <- reload_day_prep(day_folder = input$available_days_selector, basefolder = paths_day$data_root_dir)
      output$reload_sessions_available <- renderTable(ss$sessions_log)
    }

    sessions_log$log <<- ss$sessions_log
    metadata$focal_sessions_so_far <<- nrow(ss$sessions_log)
    paths_day$dirpath <- ss$dirpath
    # print(sessions_log$log)
    # print(paths_day$dirpath)

    # metadata$date <- "2000-04-04"
    #
    #
    #
  })


  # reviewing of existing data ---------------------------
  output$rev_focal_table <- renderRHandsontable(review_table_foctab(input = input, paths_day = paths_day, metadata = metadata))
  output$rev_nn <- renderRHandsontable(review_table_nn(input = input, paths_day = paths_day, metadata = metadata))
  output$rev_groom <- renderRHandsontable(review_table_groom(input = input, paths_day = paths_day, metadata = metadata))
  output$rev_aggression <- renderRHandsontable(review_table_aggr(input = input, paths_day = paths_day, metadata = metadata))








  # nearest neighbors -------------------------
  observeEvent(input$nn_scan, {
    if (metadata$session_is_active) {
      updateTabsetPanel(session, inputId = "nav_home", selected = "nearest neighbors")
      # print(nn_reactive())
    }
  })

  nn_reactive <- reactive({
    # ids <- c(all_individuals$id[all_individuals$group == input$group], c(paste0(c("AM"), 1:6), paste0(c("AF"), 1:6), paste0(c("J"), 1:6), paste0(c("I"), 1:6)))
    if (metadata$session_is_active) {
      o <- setNames(unlist(lapply(1:length(nn$ids), function(X) {
        input[[paste0("id_", nn$ids[X])]]
      })), nn$ids)
    }
  })

  observe({
    if (metadata$session_is_active) {
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
    if (metadata$session_is_active) {
      if (nn$firstrun) {
        lapply(render_nn(nn$ids, selected = nn$ini_state, sex = nn$sex, do_which = "f"), function(X) HTML(paste(X)))
      } else {
        lapply(render_nn(nn$ids, selected = nn_reactive(), sex = nn$sex, do_which = "f"), function(X) HTML(paste(X)))
      }
    }
  })
  output$nn_male <- renderUI({
    ids <- c(all_individuals$id[all_individuals$group == metadata$group], c(paste0(c("AM"), 1:6), paste0(c("AF"), 1:6), paste0(c("J"), 1:6), paste0(c("I"), 1:6)))
    if (metadata$session_is_active) {
      if (nn$firstrun) {
        lapply(render_nn(ids, selected = nn$ini_state, sex = nn$sex, do_which = "m"), function(X) HTML(paste(X)))
      } else {
        lapply(render_nn(ids, selected = nn_reactive(), sex = nn$sex, do_which = "m"), function(X) HTML(paste(X)))
      }
    }
  })
  output$nn_other <- renderUI({
    ids <- c(all_individuals$id[all_individuals$group == metadata$group], c(paste0(c("AM"), 1:6), paste0(c("AF"), 1:6), paste0(c("J"), 1:6), paste0(c("I"), 1:6)))
    if (metadata$session_is_active) {
      if (nn$firstrun) {
        lapply(render_nn(ids, selected = nn$ini_state, sex = nn$sex, do_which = "o"), function(X) HTML(paste(X)))
      } else {
        lapply(render_nn(ids, selected = nn_reactive(), sex = nn$sex, do_which = "o"), function(X) HTML(paste(X)))
      }
    }
  })


  # grooming -----------------------
  observeEvent(input$record_focal_groom_start_btn, {
    if (metadata$grooming_in_progress == TRUE) {
      modalDialog()
    } else {
      if (metadata$grooming_in_progress == FALSE & metadata$session_is_active == TRUE) showModal(focal_grooming_start_dialog(focal_id = metadata$focal_id, partners = groompartners_temp))
    }

  })
  observeEvent(input$record_focal_groom_change_btn, {
    # print(metadata$ == TRUE)
    if (metadata$grooming_in_progress == TRUE & metadata$session_is_active == TRUE) showModal(focal_grooming_change_dialog(metadata = metadata))
  })



  observeEvent(input$start_grooming, {
    metadata$grooming_in_progress <- TRUE
    metadata$grooming_direction <- input$grooming_focal_direction
    metadata$grooming_current_parter <- input$grooming_partner_name
    grooming$grooming <- grooming_table_update(grooming = grooming$grooming, event = "start", input_list = input, metadata_list = metadata)
    removeModal()
    output$debugging_groom <- renderTable(grooming$grooming[-1, ])
    metadata$grooming_withinevent_num <- metadata$grooming_withinevent_num  + 1

    # progress update
    output$focal_grooming_in_progress <- renderText(grooming_textual_message(direction = metadata$grooming_direction,
                                                                  focal_id = metadata$focal_id,
                                                                  current_grooming_parter = metadata$grooming_current_parter))

    write.csv(grooming$grooming, file = paths_sessions$current_foc_groom, row.names = FALSE, quote = FALSE)
  })

  observeEvent(input$change_grooming, {
    metadata$grooming_direction <- input$grooming_focal_direction_change
    grooming$grooming <- grooming_table_update(grooming = grooming$grooming, event = "change", input_list = input, metadata_list = metadata)
    removeModal()
    metadata$grooming_withinevent_num <- metadata$grooming_withinevent_num + 1
    output$debugging_groom <- renderTable(grooming$grooming[-1, ])

    # progress update and csv writing
    output$focal_grooming_in_progress <- renderText(grooming_textual_message(direction = metadata$grooming_direction,
                                                                             focal_id = metadata$focal_id,
                                                                             current_grooming_parter = metadata$grooming_current_parter))
    write.csv(grooming$grooming, file = paths_sessions$current_foc_groom, row.names = FALSE, quote = FALSE)
  })

  observeEvent(input$stop_grooming, {
    grooming$grooming <- grooming_table_update(grooming = grooming$grooming, event = "end", input_list = input, metadata_list = metadata)
    removeModal()
    metadata$grooming_in_progress <- FALSE
    metadata$grooming_direction <- NA
    metadata$grooming_current_parter <- NA
    metadata$grooming_withinsession_num <- metadata$grooming_withinsession_num + 1
    metadata$grooming_withinevent_num <- 1
    output$debugging_groom <- renderTable(grooming$grooming)
    write.csv(grooming$grooming, file = paths_sessions$current_foc_groom, row.names = FALSE, quote = FALSE)
  })

  # other focal session aggression -------------------
  observeEvent(input$record_focal_aggr, {
    if (metadata$session_is_active) {
      showModal(focal_aggression_dialog(focal_id = metadata$focal_id)) # submit button in dialog: 'focal_aggression'
    }
  })
  observeEvent(input$focal_aggression_abtn, {
    focal_aggression_data$aggression <- focal_aggression_dyadic_update(reactive_xdata = focal_aggression_data$aggression, input_list = input)
    write.csv(focal_aggression_data$aggression, file = paths_sessions$current_foc_aggr, row.names = FALSE, quote = FALSE)
    removeModal()
  })


  # start session ----------------------------
  observeEvent(input$start_focal_session_dialog_abtn, {
    # check whether a session is already running
    # print(paste("session is active:", metadata$session_is_active, "\n"))
    # print(paste("foctab is NULL:", is.null(v$foc_tab), "\n"))
    if (metadata$session_is_active ) {
      showModal(modalDialog(
        span("Active session detected. You can't have two sessions at the same time. Save the running session first before starting a new one."),
        footer = tagList(modalButton("Cancel"))
      ))
    } else {
      showModal(focal_start_session_dialog(potential_focals = all_individuals$id[all_individuals$group == metadata$group & all_individuals$is_focal == "yes"]))
      updateTabsetPanel(session, inputId = "nav_home", selected = "focal")
    }
  })

  observeEvent(input$focal_session_start_abtn, {
    # reset
    metadata$focal_start <- as.character(strptime(input$focal_start, format = "%Y-%m-%d %H:%M:%S"))
    metadata$focal_duration <- input$focal_duration
    metadata$focal_id <- input$focal_name

    eft <- empty_foc_table(start_time = metadata$focal_start, duration = metadata$focal_duration, id = metadata$focal_id, activity_codes = activity_codes)
    v$foctab <- eft

    metadata$progr_target = as.numeric(metadata$focal_duration)
    metadata$progr_table_lines = as.numeric(metadata$focal_duration)
    metadata$progr_na_vals = as.numeric(metadata$focal_duration)
    metadata$progr_oos = 0
    metadata$progr_act = 0
    metadata$session_is_active <- TRUE

    # update paths for per-session files
    s <- paste0(as.character(as.Date(metadata$date)), "_", metadata$focal_id, "_", metadata$observer)
    s <- paste0(s, "_", sum(sessions_log$log$focal_id == metadata$focal_id) + 1)
    metadata$current_foc_session_id <- s
    paths_sessions$current_foc_tab <- file.path(paths_day$dirpath, paste0(s, "_foctab.csv"))
    paths_sessions$current_foc_groom <- file.path(paths_day$dirpath, paste0(s, "_groom.csv"))
    paths_sessions$current_foc_aggr <- file.path(paths_day$dirpath, paste0(s, "_aggr.csv"))
    paths_sessions$current_foc_nn <- file.path(paths_day$dirpath, paste0(s, "_nn.csv"))
    # write.table(v$foctab, file = v$filename, sep = ",", row.names = FALSE, quote = FALSE, dec = ".")

    # update daily monitor
    metadata$focal_sessions_so_far <- as.numeric(metadata$focal_sessions_so_far) + 1
    sessions_log$log[metadata$focal_sessions_so_far, ] <- NA
    sessions_log$log$session_id[metadata$focal_sessions_so_far] <- metadata$current_foc_session_id
    sessions_log$log$filename[metadata$focal_sessions_so_far] <- paths_sessions$current_foc_tab
    sessions_log$log$focal_id[metadata$focal_sessions_so_far] <- metadata$focal_id
    sessions_log$log$focal_counter[metadata$focal_sessions_so_far] <- sum(sessions_log$log$focal_id == metadata$focal_id)
    sessions_log$log$session_created[metadata$focal_sessions_so_far] <- as.character(Sys.time())
    print(sessions_log$log)


    write.table(v$foctab, file = paths_sessions$current_foc_tab, sep = ",", row.names = FALSE, quote = FALSE, dec = ".")
    write.table(sessions_log$log, file = paths_day$sessions_log, sep = ",", row.names = FALSE, quote = FALSE, dec = ".")
    write.csv(data.frame(val = unlist(reactiveValuesToList(metadata))), file = paths_day$day_meta, row.names = TRUE, quote = FALSE)

    output$log <- renderTable({sessions_log$log})
    removeModal()

    # reset grooming
    metadata$grooming_in_progress <- FALSE
    metadata$grooming_direction <- NA
    metadata$grooming_current_parter <- NA
    metadata$grooming_withinsession_num <- 1
    metadata$grooming_withinevent_num <- 1
    grooming$grooming = empty_grooming()

    # reset focal aggression
    focal_aggression_data$aggression <- empty_focal_aggr()


    # set nn data
    # ids <- c(all_individuals$id[all_individuals$group == metadata$group], c(paste0(c("AM"), 1:6), paste0(c("AF"), 1:6), paste0(c("J"), 1:6), paste0(c("I"), 1:6)))
    nn$ids <- c(all_individuals$id[all_individuals$group == metadata$group], c(paste0(c("AM"), 1:6), paste0(c("AF"), 1:6), paste0(c("J"), 1:6), paste0(c("I"), 1:6)))
    nn$ini_state <- setNames(rep(FALSE, length(nn$ids)), nn$ids)
    nn$sex <- c(all_individuals$sex[all_individuals$group == metadata$group], rep("o", 24))
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
      # outtab <- hot_col(outtab, col = 1, colWidths = 0.1)
      # outtab <- hot_col(outtab, col = "time_stamp", colWidths = 0.1)

      outtab
    }
  })
  observeEvent(input$focal_table, {
    xxx <- hot_to_r(input$focal_table)
    write.table(xxx, file = paths_sessions$current_foc_tab, sep = ",", row.names = FALSE, quote = FALSE, dec = ".")
    output$static_foctab <- renderTable(xxx)


    # update progress tracker and add rows to focal table if required (automated better than manually via addrow-button)
    metadata$progr_oos = sum(xxx$activity %in% "oos")
    metadata$progr_act = sum(xxx$activity %in% activity_codes) - metadata$progr_oos
    metadata$progr_table_lines = nrow(xxx)
    metadata$progr_na_vals = sum(is.na(xxx$activity))

    if (metadata$progr_act < metadata$progr_target & !is.na(xxx$activity[nrow(xxx)])) {
      xxx <- rbind(xxx, empty_foc_table(start_time = Sys.time(), duration = 1, id = metadata$focal_id, activity_codes = activity_codes))
      xxx$sample[nrow(xxx)] <- nrow(xxx)
      xxx$time_for_display[nrow(xxx)] <- add_one_minute(xxx$time_for_display[nrow(xxx) - 1])
      metadata$progr_table_lines <- nrow(xxx) + 1
      Sys.sleep(1)
    }

    # browser()
    v$foctab <- xxx
    output$debug_foctab_progress <- renderUI({
      HTML(paste("activity samples:", metadata$progr_act, "<br>", "oos samples:", metadata$progr_oos, "<br>",
                 "NA samples:", metadata$progr_na_vals, "<br>", "target:", metadata$progr_target, "<br>", "num rows in table:", metadata$progr_table_lines))
    })
  })
  # progress tracker for session
  observeEvent(metadata$progr_act, {output$focal_dur_progress <- renderText(paste(metadata$progr_act, "of", metadata$focal_duration, "done"))})

  # end session -------------------------
  observeEvent(input$finish_focal_session, {
    # check for ongoing grooming!!!
    if (metadata$session_is_active & metadata$grooming_in_progress) {
      showModal(modalDialog("grooming still ongoing: something needs to be done"))
    }

    if (metadata$session_is_active & !metadata$grooming_in_progress) {
      temp_object <- v$foctab
      temp_object$time_stamp <- as.character(temp_object$time_stamp)
      # store focal table
      write.csv(temp_object, file = paths_sessions$current_foc_tab, row.names = FALSE, quote = FALSE)
      # store nn object
      write.csv(nn$final, file = paths_sessions$current_foc_nn, row.names = FALSE, quote = FALSE)
      # store grooming
      write.csv(grooming$grooming, file = paths_sessions$current_foc_groom, row.names = FALSE, quote = FALSE)
      # store aggression
      write.csv(focal_aggression_data$aggression, file = paths_sessions$current_foc_aggr, row.names = FALSE, quote = FALSE)
      # store sessions log
      write.table(sessions_log$log, file = paths_day$sessions_log, sep = ",", row.names = FALSE, quote = FALSE, dec = ".")

      # update list for revisions
      session_id_for_display <- paste0(sessions_log$log$focal_id, " (", sessions_log$log$session_id, ")")
      updateSelectInput(inputId = "session_for_review", choices = session_id_for_display)

      # reset reactive values object
      v$foctab = NULL # the actual data table
      metadata$focal_id <- NA
      metadata$current_foc_session_id = NA
      metadata$session_is_active = FALSE
      metadata$progr_target = as.numeric(metadata$focal_duration)
      metadata$progr_table_lines = as.numeric(metadata$focal_duration)
      metadata$progr_na_vals = as.numeric(metadata$focal_duration)
      metadata$progr_oos = 0
      metadata$progr_act = 0



      metadata$current_foc_session_id <- NA
      paths_sessions$current_foc_tab <- NA
      paths_sessions$current_foc_nn <- NA
      paths_sessions$current_foc_groom <- NA
      paths_sessions$current_foc_aggr <- NA
      updateTabsetPanel(session, inputId = "nav_home", selected = "home") # shift focus to home tab
      output$metadata <- renderPrint(isolate(unlist(reactiveValuesToList(metadata))))
    }


  })





  # app start up message and setup for day -----------------------
  startup_dialog_box(pot_observers = unique(sample(all_observers)), pot_groups = unique(all_individuals$group))

  observeEvent(input$startnewday_ok_abtn, {
    metadata$date <- as.character(input$date)
    metadata$observer <- input$observer
    metadata$group <- input$group

    # check whether data directory is there, and if not and required, create it
    paths_day$data_root_dir <- link_directory(use_dir_on_desktop = input$desktopdir)
    paths_day$dirpath <- normalizePath(file.path(paths_day$data_root_dir, paste0(as.character(metadata$date), "_", as.character(metadata$observer))), mustWork = FALSE)
    if (!dir.exists(paths_day$dirpath)) dir.create(paths_day$dirpath)
    # path names to daily data files
    filename_root <- paste0(as.character(as.Date(metadata$date)), "_global_", as.character(metadata$observer), "_0")
    paths_day$daily_census <- file.path(paths_day$dirpath, paste0(filename_root, "_census.csv"))
    paths_day$adlib_aggr <- file.path(paths_day$dirpath, paste0(filename_root, "_aggr.csv"))
    paths_day$sessions_log <- file.path(paths_day$dirpath, paste0(filename_root, "_log.csv"))
    paths_day$day_meta <- file.path(paths_day$dirpath, paste0(filename_root, "_meta.csv"))
    output$info_paths_day <- renderPrint(isolate(reactiveValuesToList(paths_day))) # diagnostics/info


    individuals$for_nn <- xdata$presence[xdata$presence$group == metadata$group, ]
    xdata$presence <- xdata$presence[xdata$presence$group == metadata$group, ] # select relevant group...
    metadata$get_started <- TRUE
    output$dategroupobs <- renderText({
      paste("<p>selected group:", "<b style='color:red'>", metadata$group, "</b></p>", "<hr>",
            "<p>selected date:<b>", as.character(metadata$date), "</b></p>", "<hr>",
            "<p>selected observer:<b>", as.character(metadata$observer), "</b></p>")
    })
    removeModal()

    # write files
    write.csv(data.frame(val = unlist(reactiveValuesToList(metadata))), file = paths_day$day_meta, row.names = TRUE, quote = FALSE)
  })


  # census related ---------------------
  observeEvent(input$addgrouptocensus, {
    showModal(additional_group_for_census(all_groups = unique(all_individuals$group), current_group = metadata$group))
  })
  observeEvent(input$add_group_selected_submit, {
    if (!is.null(xdata$presence) & metadata$get_started == TRUE) {
      xdata$presence <- rbind(xdata$presence, all_individuals[all_individuals$group == input$add_group_selected, ])
    }
    removeModal()
  })
  observeEvent(input$census_table, {
    # print(paste("presence is NULL:", is.null(xdata$presence), "\n"))
    # print(paste("day got started:", metadata$get_started, "\n"))
    xxx <- hot_to_r(input$census_table)
    # print(paste("nrow presence:", nrow(xxx), "\n"))
    if (!is.null(xdata$presence) & metadata$get_started == TRUE) {
      xxx <- hot_to_r(input$census_table)
      write.table(xxx, file = paths_day$daily_census, sep = ",", row.names = FALSE, quote = FALSE, dec = ".")
    }
  })
  observeEvent(input$addnewrowtocensus, {
    if (!is.null(xdata$presence) & metadata$get_started == TRUE) {
      xxx <- hot_to_r(input$census_table)
      xxx <- rbind(xxx, NA)
      xxx$present[nrow(xxx)] <- FALSE
      xxx$group[nrow(xxx)] <- xxx$group[1]
      xdata$presence <- xxx
    }
  })


  output$census_table <- renderRHandsontable({
    if (!is.null(xdata$presence) & metadata$get_started == TRUE) {
      xtab <- rhandsontable(xdata$presence, rowHeaders = NULL)
      # make certain cells/columns read-only
      xtab <- hot_col(xtab, col = "sex", readOnly = TRUE)
      swell_col <- which(colnames(xdata$presence) == "swelling")
      for (i in which(xdata$presence$sex == "m")) xtab <- hot_cell(xtab, row = i, col = swell_col, readOnly = TRUE)
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
    adlib_agg$dyadic <- adlib_aggression_dyadic_update(reactive_xdata = adlib_agg$dyadic, input_list = input)
    write.csv(adlib_agg$dyadic, file = paths_day$adlib_aggr, quote = FALSE, row.names = FALSE)
    removeModal()
  })

  observeEvent(input$go_to_census_btn, {
    updateTabsetPanel(session, inputId = "nav_home", selected = "census") # shift focus to census tab
  })

  # simple debuggging/diagnostics elements
  output$debug_adlib_aggression <- renderTable(adlib_agg$dyadic)
  output$rsession_info <- renderPrint(sessionInfo())
  output$current_wd <- renderPrint(getwd())

  observeEvent(input$open_data_dir_abtn, {
    if (dir.exists(paths_day$data_root_dir)) system2("open", args = shQuote(paths_day$data_root_dir))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
