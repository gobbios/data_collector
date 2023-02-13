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
source("helpers/id_table.R")
source("helpers/info_and_debug.R")
source("helpers/metadata_reset_after_focal.R")

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

# list with pairs matching input names and data table column names
mdata <- read.csv("data_name_matching.csv", stringsAsFactors = FALSE)

# list with observers (to be outsourced to csv file eventually)
all_observers <- c("petterson", "maria", "carel", "jeanne", "robert", "joan", "julia")
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
                             actionButton("open_data_dir_abtn", "open data directory"),
                             actionButton("show_metadata_abtn", "show meta data")
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
                      actionButton("addgrouptocensus", "additional group"),
                      rHandsontableOutput("census_table_additional_group")
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
                      # span(HTML("<p style='color:Khaki;'>This particular tab here is still work in progress.</p>")),
                      span(HTML("<p>Here you can review focal sessions that were done on this day. If there is an active session it is selected by default. If there is no finished session, nothing is displayed.</p>")),
                      span(HTML("<p>Also, here you can see and review adlib data (data collected outside a focal session, i.e. for now only 'adlib' aggression).</p>")),
                      selectInput("session_for_review", label = "select session", choices = c("")),
                      navlistPanel(widths = c(2, 10),
                                   tabPanel("focal (point samples)",
                                            rHandsontableOutput("rev_focal_table")),
                                   tabPanel("aggression",
                                            rHandsontableOutput("rev_aggression")),
                                   tabPanel("grooming",
                                            rHandsontableOutput("rev_groom")),
                                   tabPanel("neighbors",
                                            rHandsontableOutput("rev_nn")),
                                   tabPanel("adlib aggression",
                                            rHandsontableOutput("rev_adlib_aggression")),
                                   
                                   
                      )
             ),

             tabPanel("diagnostics",
                      # h4("file paths to daily files:"),
                      # verbatimTextOutput("info_paths_day"),
                      h4("focal session overview:"),
                      tableOutput("log"),
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
             ),
             tabPanel("read me", includeMarkdown("manual.Rmd")) # withMathJax(includeMarkdown("manual.Rmd"))
  )
)

server <- function(input, output, session) {
  print(getwd())
  
  
  # get a conditional panel (grooming progress indicator) dependent on reactive values in the server
  output$panelStatus <- reactive({
    metadata$grooming_in_progress
  })
  outputOptions(output, "panelStatus", suspendWhenHidden = FALSE)

  observeEvent(input$test_stuff, {
    print(unlist(isolate(reactiveValuesToList(v))$progress))
    print(unlist(isolate(reactiveValuesToList(metadata))))
  })

  # metadata (info regarding day): list with names
  # once per day: group date observer focal_sessions_so_far; also paths to day's directory and global files (census, adlib aggr, session log, stored metadata)
  # per focal session: focal_start focal_name get_started session_is_active current_foc_session_id
  # per focal session: paths to four files (point samples, grooming, nn, aggr)
  # progress tracker within a focal session (activity counter etc...)
  # tracker for grooming
  metadata <- empty_metadata()

  # v: for a single focal session (point samples)
  v <- reactiveValues(foctab = NULL) # the actual data table
  
  # monitor sessions across day
  sessions_log <- reactiveValues(log = empty_log())
  # adlib aggression data
  adlib_agg <- reactiveValues(dyadic = empty_adlib_aggr())
  # focal session grooming
  grooming <- reactiveValues(grooming = empty_grooming())
  # focal session aggression
  focal_aggression_data <- reactiveValues(aggression = empty_focal_aggr())

  # census
  census <- reactiveValues(census = NULL)
  # NN (new style)
  nn_data <- reactiveValues(nn_data = NULL)
  nn_for_storage <- reactiveValues(nn_for_storage = empty_nn_storage())

  # reload data -------------------------
  observeEvent(input$reload_day_dialog_btn, {
    reload_day_dialog_box()
    metadata$data_root_dir <- link_directory(use_dir_on_desktop = input$desktopdir)
    ad <- reload_list_days(metadata$data_root_dir)
    updateSelectInput(session, inputId = "available_days_selector_new", choices = ad$day_folder_display[!ad$empty])
  })
  observeEvent(input$reload_day_cancel_abtn, {
    startup_dialog_box(pot_observers = unique(sample(all_observers)), pot_groups = unique(all_individuals$group))
  })
  observeEvent(input$reload_day_doit_abtn, {
    removeModal()
    if (!is.null(input$available_days_selector_new) & input$available_days_selector_new != "") {
      # tried to outsource this into a function, but not yet successfully (read_meta_2)
      
      xpaths <- list.files(file.path(metadata$data_root_dir, input$available_days_selector_new), full.names = TRUE, pattern = "meta.csv$")
      print(xpaths)
      if (length(xpaths) != 1) stop("didn't find exactly one ")
      x <- read.csv(xpaths, row.names = 1)
      
      # paths to daily info
      metadata$data_root_dir <- x["data_root_dir", 1]
      metadata$day_dir <- x["day_dir", 1]
      metadata$daily_census <- x["daily_census", 1]
      metadata$adlib_aggr <- x["adlib_aggr", 1]
      metadata$sessions_log <- x["sessions_log", 1]
      metadata$day_meta <- x["day_meta", 1]
      
      # paths to active session if any
      metadata$active_foc_tab <- x["active_foc_tab", 1]
      metadata$active_foc_nn <- x["active_foc_nn", 1]
      metadata$active_foc_groom <- x["active_foc_groom", 1]
      metadata$active_foc_aggr <- x["active_foc_aggr", 1]
      
      # actual meta data for day
      metadata$date <- x["date", 1]
      metadata$observer <- x["observer", 1]
      metadata$group <- x["group", 1]
      metadata$get_started <- as.logical(x["get_started", 1])
      metadata$focal_sessions_so_far <- as.numeric(x["focal_sessions_so_far", 1])
      # current focal session
      metadata$focal_duration <- as.numeric(x["focal_duration", 1])
      metadata$focal_id <- x["focal_id", 1]
      metadata$focal_start <- x["focal_start", 1]
      metadata$focal_start_hour <- as.numeric(x["focal_start_hour", 1])
      metadata$focal_start_minute <- as.numeric(x["focal_start_minute", 1])
      metadata$session_is_active <- as.logical(x["session_is_active", 1])
      metadata$current_foc_session_id <- x["current_foc_session_id", 1]
      # progress within the current focal session
      metadata$progr_target <- as.numeric(x["progr_target", 1])
      metadata$progr_table_lines <- as.numeric(x["progr_table_lines", 1])
      metadata$progr_na_vals <- as.numeric(x["progr_na_vals", 1])
      metadata$progr_oos <- as.numeric(x["progr_oos", 1])
      metadata$progr_act <- as.numeric(x["progr_act", 1])
      metadata$nn_scan_no <- as.numeric(x["nn_scan_no", 1])
      # grooming monitor
      metadata$grooming_in_progress <- as.logical(x["grooming_in_progress", 1])
      metadata$grooming_direction <- x["grooming_direction", 1]
      metadata$grooming_current_parter <- x["grooming_current_parter", 1]
      metadata$grooming_withinsession_num <- as.numeric(x["grooming_withinsession_num", 1])
      metadata$grooming_withinevent_num <- as.numeric(x["grooming_withinevent_num", 1])
      # editing monitor
      metadata$edit_adlib_aggr <- as.numeric(x["edit_adlib_aggr", 1])
      
      # update reactive objects
      # if there is an active focal session
      if (!is.na(metadata$active_foc_tab)) v$foctab <- read.csv(metadata$active_foc_tab)
      if (!is.na(metadata$active_foc_groom)) grooming$grooming <- read.csv(metadata$active_foc_groom)
      if (!is.na(metadata$active_foc_aggr)) focal_aggression_data$aggression <- read.csv(metadata$active_foc_aggr)
      if (!is.na(metadata$active_foc_nn)) {
        nn_data$nn_data <- read.csv(file = gsub(pattern = ".csv$", replacement = "_temp.csv", metadata$active_foc_nn))
        nn_for_storage$nn_for_storage <- read.csv(file = metadata$active_foc_nn)
      }
      # and daily stuff that should be there regardless
      census$census <- read.csv(file = metadata$daily_census)
      sessions_log$log <- read.csv(file = metadata$sessions_log)
      adlib_agg$dyadic <- read.csv(file = metadata$adlib_aggr)
      
      # reviewing pane
      session_id_for_display <- paste0(sessions_log$log$focal_id, " (", sessions_log$log$session_id, ")")
      updateSelectInput(inputId = "session_for_review", choices = session_id_for_display, selected = rev(session_id_for_display)[1])
      
      # grooming status
      if (metadata$grooming_in_progress) {
        # progress update
        output$focal_grooming_in_progress <- renderText(grooming_textual_message(direction = metadata$grooming_direction,
                                                                                 focal_id = metadata$focal_id,
                                                                                 current_grooming_parter = metadata$grooming_current_parter))
      }
      # home panel info
      output$dategroupobs <- renderText({
        paste("<p>selected group:", "<b style='color:red'>", metadata$group, "</b></p>", "<hr>",
              "<p>selected date:<b>", as.character(metadata$date), "</b></p>", "<hr>",
              "<p>selected observer:<b>", as.character(metadata$observer), "</b></p>")
      })
      
    }
  })
  
  observeEvent(input$copy_examples_abtn, {
    x <- list.files("examples_sessions", full.names = TRUE, include.dirs = TRUE)
    print(basename(x))
    for (i in 1:length(x)) {
      if (dir.exists(file.path("www", basename(x)[i]))) {
        print(list.files(file.path("www", basename(x)[i]), full.names = TRUE))
        file.remove(list.files(file.path("www", basename(x)[i]), full.names = TRUE))
      } 
      dir.create(file.path("www", basename(x)[i]), showWarnings = FALSE)
      y <- list.files(x[i], full.names = TRUE)
      for (k in y) file.copy(from = k, to = file.path("www", basename(x)[i], basename(k)), overwrite = TRUE)
    }
    ad <- reload_list_days(metadata$data_root_dir)
    updateSelectInput(session, inputId = "available_days_selector_new", choices = ad$day_folder_display[!ad$empty])
  })
  
  # reviewing of existing data ---------------------------
  observeEvent(input$focal_table, output$rev_focal_table <- renderRHandsontable(review_table_foctab(input = input, metadata = metadata)))
  observeEvent(grooming$grooming, output$rev_groom <- renderRHandsontable(review_table_groom(input = input, metadata = metadata)))
  observeEvent(nn_for_storage$nn_for_storage, output$rev_nn <- renderRHandsontable(review_table_nn(input = input, metadata = metadata)))


  # nearest neighbors -------------------------
  observeEvent(input$nn_scan, {
    if (metadata$session_is_active) {
      updateTabsetPanel(session, inputId = "nav_home", selected = "nearest neighbors")
    }
  })

  nn_reactive <- reactive({
    if (metadata$session_is_active) {
      setNames(unlist(lapply(nn_data$nn_data$id, function(X) {
        input[[paste0("id_", X)]]
      })), nn_data$nn_data$id[paste0("id_", nn_data$nn_data$id) %in% names(input)])
    }
  })

  # update current nn tracker
  observe({
    if (metadata$session_is_active) {
      # if (any(grepl("^id_", names(input)))) print(isolate(nn_reactive()))
      lapply(nn_data$nn_data$id, function(X) {
        observeEvent(input[[paste0("id_", X)]], {
          xxx <- nn_reactive()
          xxx <- xxx[nn_data$nn_data$id]
          nn_data$nn_data$in_nn_tracker <<- xxx
        })
      })
      # cat("--------in 'observe' - nn:---------\n")
      # if (!is.null(nn_data$nn_data)) cat_table(nn_data$nn_data, head = FALSE)
      # cat(unlist(lapply(isolate(nn_data$nn_data$id), function(X)input[[paste0("id_", X)]])), "\n")
    }
  })

  observeEvent(input$submit_nn, {
    if (metadata$session_is_active) {
      df <- data.frame(session_id = metadata$current_foc_session_id,
                       scan_no = as.integer(metadata$nn_scan_no + 1),
                       id = nn_data$nn_data$id,
                       present = nn_data$nn_data$in_nn_tracker)
      if (nrow(nn_for_storage$nn_for_storage) == 0) {
        nn_for_storage$nn_for_storage[1:nrow(df), ] <- df
      } else {
        nn_for_storage$nn_for_storage <- rbind(nn_for_storage$nn_for_storage, df)
      }
      metadata$nn_scan_no <- metadata$nn_scan_no + 1
      # write file now as well
      write.csv(nn_for_storage$nn_for_storage, file = metadata$active_foc_nn, row.names = FALSE, quote = FALSE)
      write.csv(nn_data$nn_data, file = gsub(pattern = ".csv$", replacement = "_temp.csv", metadata$active_foc_nn), row.names = FALSE, quote = FALSE)
      # reset
      nn_data$nn_data$in_nn_tracker <- FALSE
      lapply(nn_data$nn_data$id, function(X) {
        updateCheckboxInput(inputId = paste0("id_", X), value = FALSE)
      })
    }
    updateTabsetPanel(session, inputId = "nav_home", selected = "focal") # shift focus to home tab
  })





  output$nn_fem <- renderUI({
    if (metadata$session_is_active) {
      lapply(render_nn(nn_data$nn_data$id, selected = nn_data$nn_data$in_nn_tracker, sex = nn_data$nn_data$sex, do_which = "f"), function(X) HTML(paste(X)))
    }
  })
  output$nn_male <- renderUI({
    if (metadata$session_is_active) {
      lapply(render_nn(nn_data$nn_data$id, selected = nn_data$nn_data$in_nn_tracker, sex = nn_data$nn_data$sex, do_which = "m"), function(X) HTML(paste(X)))
    }
  })
  output$nn_other <- renderUI({
    if (metadata$session_is_active) {
      lapply(render_nn(nn_data$nn_data$id, selected = nn_data$nn_data$in_nn_tracker, sex = nn_data$nn_data$sex, do_which = "o"), function(X) HTML(paste(X)))
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
    output$debugging_groom <- renderTable(grooming$grooming)
    metadata$grooming_withinevent_num <- metadata$grooming_withinevent_num  + 1

    # progress update
    output$focal_grooming_in_progress <- renderText(grooming_textual_message(direction = metadata$grooming_direction,
                                                                  focal_id = metadata$focal_id,
                                                                  current_grooming_parter = metadata$grooming_current_parter))

    write.csv(grooming$grooming, file = metadata$active_foc_groom, row.names = FALSE, quote = FALSE)
    write.csv(data.frame(val = unlist(reactiveValuesToList(metadata))), file = metadata$day_meta, row.names = TRUE, quote = FALSE)
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
    write.csv(grooming$grooming, file = metadata$active_foc_groom, row.names = FALSE, quote = FALSE)
    write.csv(data.frame(val = unlist(reactiveValuesToList(metadata))), file = metadata$day_meta, row.names = TRUE, quote = FALSE)
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
    write.csv(grooming$grooming, file = metadata$active_foc_groom, row.names = FALSE, quote = FALSE)
    write.csv(data.frame(val = unlist(reactiveValuesToList(metadata))), file = metadata$day_meta, row.names = TRUE, quote = FALSE)
    
  })

  # focal session aggression -------------------
  observeEvent(input$record_focal_aggr, {
    if (metadata$session_is_active) {
      showModal(focal_aggression_dialog(focal_id = metadata$focal_id)) # submit button in dialog: 'focal_aggression'
    }
  })
  observeEvent(input$focal_aggression_abtn, {
    focal_aggression_data$aggression <- focal_aggression_dyadic_update(reactive_xdata = focal_aggression_data$aggression, input_list = input, match_table = mdata, what_row = metadata$edit_focal_aggression)
    write.csv(focal_aggression_data$aggression, file = metadata$active_foc_aggr, row.names = FALSE, quote = FALSE)
    removeModal()
    metadata$edit_focal_aggression <- NA
    
  })
  
  # display table for review
  observeEvent(focal_aggression_data$aggression, output$rev_aggression <- renderRHandsontable(review_table_aggr(input = input, metadata = metadata)))
  # edit table in reviewing pane
  observeEvent(input$rev_aggression_select$select$c, {
    # print(input$rev_adlib_aggression_select$select$c)
    which_col <- which(colnames(hot_to_r(input$rev_aggression)) == "action")
    x <- input$rev_aggression_select$select$c
    print(which_col)
    if (!is.na(x) & length(which_col) == 1) {
      if (x == which_col) {
        matchdata <- mdata[mdata$context == "focal_agg", ]
        metadata$edit_focal_aggression <- input$rev_aggression_select$select$r
        print(metadata$edit_focal_aggression)
        for (i in 1:nrow(matchdata)) updateTextInput(inputId = unname(matchdata[i, "inputname"]),
                                                              value = unname(focal_aggression_data$aggression[metadata$edit_focal_aggression, matchdata[i, "tabcol"]]))
        showModal(focal_aggression_dialog(focal_id = focal_aggression_data$aggression[metadata$edit_focal_aggression, "focal"]))
        
        x <- NA
      }
    }
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
      metadata$focal_start_hour = curtime()[1]
      metadata$focal_start_minute = curtime()[2]
    }
  })
  
  

  observeEvent(input$focal_session_start_abtn, {
    # reset
    metadata$focal_start <- as.character(strptime(paste0(metadata$date, " ", metadata$focal_start_hour, ":", metadata$focal_start_minute, ":00"), format = "%Y-%m-%d %H:%M:%S"))
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
    metadata$active_foc_tab <- file.path(metadata$day_dir, paste0(s, "_foctab.csv"))
    metadata$active_foc_nn <- file.path(metadata$day_dir, paste0(s, "_nn.csv"))
    metadata$active_foc_groom <- file.path(metadata$day_dir, paste0(s, "_groom.csv"))
    metadata$active_foc_aggr <- file.path(metadata$day_dir, paste0(s, "_aggr.csv"))
    # update daily monitor
    metadata$focal_sessions_so_far <- as.numeric(metadata$focal_sessions_so_far) + 1
    sessions_log$log[metadata$focal_sessions_so_far, ] <- NA
    sessions_log$log$session_id[metadata$focal_sessions_so_far] <- metadata$current_foc_session_id
    sessions_log$log$focal_id[metadata$focal_sessions_so_far] <- metadata$focal_id
    sessions_log$log$focal_counter[metadata$focal_sessions_so_far] <- sum(sessions_log$log$focal_id == metadata$focal_id)
    sessions_log$log$session_created[metadata$focal_sessions_so_far] <- as.character(Sys.time())
    sessions_log$log$path_foc_tab[metadata$focal_sessions_so_far] <- metadata$active_foc_tab
    sessions_log$log$path_foc_nn[metadata$focal_sessions_so_far] <- metadata$active_foc_nn
    sessions_log$log$path_foc_groom[metadata$focal_sessions_so_far] <- metadata$active_foc_groom
    sessions_log$log$path_foc_aggr[metadata$focal_sessions_so_far] <- metadata$active_foc_aggr
    cat_table(sessions_log$log)

    write.table(v$foctab, file = metadata$active_foc_tab, sep = ",", row.names = FALSE, quote = FALSE, dec = ".")
    write.table(sessions_log$log, file = metadata$sessions_log, sep = ",", row.names = FALSE, quote = FALSE, dec = ".")
    write.csv(data.frame(val = unlist(reactiveValuesToList(metadata))), file = metadata$day_meta, row.names = TRUE, quote = FALSE)

    removeModal()

    # start new grooming table (reset old one)
    metadata$grooming_in_progress <- FALSE
    metadata$grooming_direction <- NA
    metadata$grooming_current_parter <- NA
    metadata$grooming_withinsession_num <- 1
    metadata$grooming_withinevent_num <- 1
    grooming$grooming <- empty_grooming()
    write.csv(grooming$grooming, file = metadata$active_foc_groom, row.names = FALSE, quote = FALSE)
    
    # start new focal aggression (reset old one)
    focal_aggression_data$aggression <- empty_focal_aggr()
    write.csv(focal_aggression_data$aggression, file = metadata$active_foc_aggr, row.names = FALSE, quote = FALSE)
    
    # initiate nn scan table
    nn_data$nn_data <- id_table_initiate(all_individuals, group = metadata$group, n_age_sex_classes = 1, include_nn_ids = TRUE)
    # and update from current census
    nn_data$nn_data <- ammend_nn_from_census(nn = nn_data$nn_data, census = hot_to_r(input$census_table))
    # initial nn_storage object for given session
    nn_for_storage$nn_for_storage <- empty_nn_storage()
    metadata$nn_scan_no <- 0
    write.csv(nn_for_storage$nn_for_storage, file = metadata$active_foc_nn, row.names = FALSE, quote = FALSE)
    write.csv(nn_data$nn_data, file = gsub(pattern = ".csv$", replacement = "_temp.csv", metadata$active_foc_nn), row.names = FALSE, quote = FALSE)
    
    # reviewing pane
    session_id_for_display <- paste0(sessions_log$log$focal_id, " (", sessions_log$log$session_id, ")")
    updateSelectInput(inputId = "session_for_review", choices = session_id_for_display, selected = rev(session_id_for_display)[1])
    
  })
  
  # start session: time display----------------
  observe( {
    if (!is.na(metadata$focal_start_hour) & !is.na(metadata$focal_start_minute)) {
      x <- paste0(sprintf("%02.f", metadata$focal_start_hour), 
                  ":", 
                  sprintf("%02.f", metadata$focal_start_minute))
      output$focal_session_time_val <- renderUI(HTML(paste(x)))
    }
  })
  observeEvent(input$incr, {
    x <- update_time_display(metadata, "up")
    metadata$focal_start_minute <- x[2]
    metadata$focal_start_hour <- x[1]
  })
  observeEvent(input$decr, {
    x <- update_time_display(metadata, "down")
    metadata$focal_start_minute <- x[2]
    metadata$focal_start_hour <- x[1]
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
    write.table(xxx, file = metadata$active_foc_tab, sep = ",", row.names = FALSE, quote = FALSE, dec = ".")
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
  observeEvent(metadata$progr_act, {
    if (metadata$session_is_active) {
      output$focal_dur_progress <- renderText(paste(metadata$progr_act, "of", metadata$focal_duration, "done"))
    }
  })

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
      write.csv(temp_object, file = metadata$active_foc_tab, row.names = FALSE, quote = FALSE)
      # store nn object
      write.csv(nn_for_storage$nn_for_storage, file = metadata$active_foc_nn, row.names = FALSE, quote = FALSE)
      write.csv(nn_data$nn_data, file = gsub(pattern = ".csv$", replacement = "_temp.csv", metadata$active_foc_nn), row.names = FALSE, quote = FALSE)
      # store grooming
      write.csv(grooming$grooming, file = metadata$active_foc_groom, row.names = FALSE, quote = FALSE)
      # store aggression
      write.csv(focal_aggression_data$aggression, file = metadata$active_foc_aggr, row.names = FALSE, quote = FALSE)
      # store sessions log
      write.table(sessions_log$log, file = metadata$sessions_log, sep = ",", row.names = FALSE, quote = FALSE, dec = ".")

      # update list for revisions
      session_id_for_display <- paste0(sessions_log$log$focal_id, " (", sessions_log$log$session_id, ")")
      updateSelectInput(inputId = "session_for_review", choices = session_id_for_display, selected = rev(session_id_for_display)[1])

      # reset reactive values objects
      v$foctab = NULL # the actual data table
      # and metadata (including file paths) pertaining to focal sessions
      
      
      metadata <- metadata_reset_after_focal(metadata)
      # metadata$focal_id <- NA
      # metadata$current_foc_session_id <- NA
      # metadata$session_is_active <- FALSE
      # metadata$focal_start <- NA
      # metadata$focal_start_hour <- NA
      # metadata$focal_start_minute <- NA
      
      # metadata$progr_target <- NA
      # metadata$progr_table_lines <- NA
      # metadata$progr_na_vals <- NA
      # metadata$progr_oos <- NA
      # metadata$progr_act <- NA
      # metadata$nn_scan_no <- NA
      # metadata$grooming_in_progress <- FALSE
      # metadata$grooming_direction <- NA
      # metadata$grooming_current_parter <- NA
      # metadata$grooming_withinsession_num <- 1
      # metadata$grooming_withinevent_num <- 1
      # 
      # metadata$current_foc_session_id <- NA
      # metadata$active_foc_tab <- NA
      # metadata$active_foc_nn <- NA
      # metadata$active_foc_groom <- NA
      # metadata$active_foc_aggr <- NA
      
      
      updateTabsetPanel(session, inputId = "nav_home", selected = "home") # shift focus to home tab
      write.csv(data.frame(val = unlist(reactiveValuesToList(metadata))), file = metadata$day_meta, row.names = TRUE, quote = FALSE)
    }
  })





  # app start up message and setup for day -----------------------
  startup_dialog_box(pot_observers = unique(sample(all_observers)), pot_groups = unique(all_individuals$group))

  observeEvent(input$startnewday_ok_abtn, {
    metadata$date <- as.character(input$date)
    metadata$observer <- input$observer
    metadata$group <- input$group

    # check whether data directory is there, and if not and required, create it
    metadata$data_root_dir <- link_directory(use_dir_on_desktop = input$desktopdir)
    metadata$day_dir <- normalizePath(file.path(metadata$data_root_dir, 
                                                paste0(as.character(metadata$date), "_", as.character(metadata$observer))), mustWork = FALSE)
    filename_root <- paste0(as.character(as.Date(metadata$date)), "_global_", as.character(metadata$observer), "_0")
    metadata$daily_census <- file.path(metadata$day_dir, paste0(filename_root, "_census.csv"))
    metadata$adlib_aggr <- file.path(metadata$day_dir, paste0(filename_root, "_aggr.csv"))
    metadata$sessions_log <- file.path(metadata$day_dir, paste0(filename_root, "_log.csv"))
    metadata$day_meta <- file.path(metadata$day_dir, paste0(filename_root, "_meta.csv"))
    if (!dir.exists(metadata$day_dir)) dir.create(metadata$day_dir)
    
    
    # paths_day$data_root_dir <- link_directory(use_dir_on_desktop = input$desktopdir)
    # paths_day$dirpath <- normalizePath(file.path(paths_day$data_root_dir, paste0(as.character(metadata$date), "_", as.character(metadata$observer))), mustWork = FALSE)
    # if (!dir.exists(paths_day$dirpath)) dir.create(paths_day$dirpath)
    # path names to daily data files
    # filename_root <- paste0(as.character(as.Date(metadata$date)), "_global_", as.character(metadata$observer), "_0")
    # paths_day$daily_census <- file.path(paths_day$dirpath, paste0(filename_root, "_census.csv"))
    # paths_day$adlib_aggr <- file.path(paths_day$dirpath, paste0(filename_root, "_aggr.csv"))
    # paths_day$sessions_log <- file.path(paths_day$dirpath, paste0(filename_root, "_log.csv"))
    # paths_day$day_meta <- file.path(paths_day$dirpath, paste0(filename_root, "_meta.csv"))
    # output$info_paths_day <- renderPrint(isolate(reactiveValuesToList(paths_day))) # diagnostics/info

    # initiate census table
    census$census <- id_table_initiate(all_individuals, group = metadata$group, include_nn_ids = FALSE)

    metadata$get_started <- TRUE
    output$dategroupobs <- renderText({
      paste("<p>selected group:", "<b style='color:red'>", metadata$group, "</b></p>", "<hr>",
            "<p>selected date:<b>", as.character(metadata$date), "</b></p>", "<hr>",
            "<p>selected observer:<b>", as.character(metadata$observer), "</b></p>")
    })
    removeModal()

    # write files
    write.csv(data.frame(val = unlist(reactiveValuesToList(metadata))), file = metadata$day_meta, row.names = TRUE, quote = FALSE)
    write.table(census$census, file = metadata$daily_census, sep = ",", row.names = FALSE, quote = FALSE, dec = ".")
    write.table(sessions_log$log, file = metadata$sessions_log, sep = ",", row.names = FALSE, quote = FALSE, dec = ".")
    write.csv(adlib_agg$dyadic, file = metadata$adlib_aggr, quote = FALSE, row.names = FALSE)
    
    output$log <- renderTable(sessions_log$log[, 1:4])
    
  })


  # census related ---------------------
  observeEvent(input$addgrouptocensus, {
    if (isTRUE(metadata$session_is_active)) {
      showModal(modalDialog(
        "Can't add group to census during an active focal session.",
        easyClose = TRUE,
        footer = NULL
      ))
    } else {
      showModal(additional_group_for_census(all_groups = unique(all_individuals$group), current_group = metadata$group))
    }
    
  })
  observeEvent(input$add_group_selected_submit, {
    if (!is.null(census$census) & metadata$get_started == TRUE & isFALSE(metadata$session_is_active)) {
      xxx <- id_table_initiate(xdata = all_individuals, group = input$add_group_selected, include_nn_ids = FALSE)
      xxx <- xxx[!xxx$id %in% c("new1", "new2"), ]
      cat_table(xxx, head = FALSE)
      # census$census <- rbind(census$census, all_individuals[all_individuals$group == input$add_group_selected, ])
      census$census <- rbind(census$census, xxx)
      
    }
    
    removeModal()
  })
  observeEvent(input$census_table, {
    xxx <- hot_to_r(input$census_table)
    # print(paste("nrow presence:", nrow(xxx), "\n"))
    if (!is.null(census$census) & metadata$get_started == TRUE) {
      xxx <- hot_to_r(input$census_table)
      write.table(xxx, file = metadata$daily_census, sep = ",", row.names = FALSE, quote = FALSE, dec = ".")
      # update nn table if necessary
      nn_data$nn_data <- ammend_nn_from_census(nn = nn_data$nn_data, census = xxx)
    }
  })
  observeEvent(input$addnewrowtocensus, {
    if (!is.null(census$census) & metadata$get_started == TRUE) {
      census$census <- hot_to_r(input$census_table)
      census$census <- rbind(census$census, NA)
    }
  })


  output$census_table <- renderRHandsontable({
    if (!is.null(census$census) & metadata$get_started == TRUE) {
      xtab <- rhandsontable(census$census, rowHeaders = NULL)
      # make certain cells/columns read-only
      xtab <- hot_col(xtab, col = "sex", readOnly = TRUE)
      swell_col <- which(colnames(census$census) == "swelling")
      for (i in which(census$census$sex == "m")) xtab <- hot_cell(xtab, row = i, col = swell_col, readOnly = TRUE)
      # xtab <- rhandsontable(census$census[, -c(which(colnames(census$census) %in% c("is_focal", "sex")))], rowHeaders = NULL)
      # xtab <- hot_row(xtab, c(1,3, 5), readOnly = TRUE)
      # xtab <- hot_col(xtab, c(1), readOnly = TRUE)
      # xtab <- hot_col(xtab, c(1), readOnly = TRUE)
      hot_table(xtab, highlightCol = TRUE, highlightRow = TRUE)
    }
  })
  
  
  # adlib aggression -----------------
  # data entry dialog
  observeEvent(input$adlib_aggression_dialog, {
    showModal(adlib_aggression_dyadic_dialog()) # submit button in dialog: 'adlib_aggression'
  })
  # save and write input
  observeEvent(input$adlib_aggression, {
    adlib_agg$dyadic <- adlib_aggression_dyadic_update(reactive_xdata = adlib_agg$dyadic, input_list = input, what_row = metadata$edit_adlib_aggr, match_table = mdata)
    write.csv(adlib_agg$dyadic, file = metadata$adlib_aggr, quote = FALSE, row.names = FALSE)
    removeModal()
    metadata$edit_adlib_aggr <- NA
  })
  # display table for review
  observeEvent(adlib_agg$dyadic, output$rev_adlib_aggression <- renderRHandsontable(review_table_adlib_agg(metadata = metadata)))
  # edit table in reviewing pane
  observeEvent(input$rev_adlib_aggression_select$select$c, {
    # print(input$rev_adlib_aggression_select$select$c)
    which_col <- which(colnames(hot_to_r(input$rev_adlib_aggression)) == "action")
    x <- input$rev_adlib_aggression_select$select$c
    # print(which_col)
    if (!is.na(x) & length(which_col) == 1) {
      if (x == which_col) {
        matchdata <- mdata[mdata$context == "adlib_agg", ]
        metadata$edit_adlib_aggr <- input$rev_adlib_aggression_select$select$r
        showModal(adlib_aggression_dyadic_dialog())
        for (i in 1:nrow(matchdata)) updateTextInput(inputId = unname(matchdata[i, "inputname"]), 
                                                              value = unname(adlib_agg$dyadic[metadata$edit_adlib_aggr, matchdata[i, "tabcol"]]))
        x <- NA
      }
    }
  })
  
  
  
  # navigation ---------------------------
  observeEvent(input$go_to_census_btn, {
    updateTabsetPanel(session, inputId = "nav_home", selected = "census") # shift focus to census tab
  })
  
  # debugging and info ------------------------
  # display current metadata in popup
  observeEvent(input$show_metadata_abtn, {
    showModal(modalDialog(
      # title = "",
      easyClose = TRUE,
      fluidRow(
        column(5, htmlOutput("metadata_info_out1"), style = "border: 1px solid grey; margin: 10px; padding: 2px; border-radius: 10px"), 
        column(5, htmlOutput("metadata_info_out2"), style = "border: 1px solid grey; margin: 10px; padding: 2px; border-radius: 10px")
      ),
      # hr(),
      fluidRow(
        column(5, htmlOutput("metadata_info_out3"), style = "border: 1px solid grey; margin: 10px; padding: 2px; border-radius: 10px"), 
        column(5, htmlOutput("metadata_info_out4"), style = "border: 1px solid grey; margin: 10px; padding: 2px; border-radius: 10px")
      ),
      fluidRow(
        column(11, htmlOutput("metadata_info_out5"), style = "border: 1px solid grey; margin: 10px; padding: 2px; border-radius: 5px")
      ),
      fluidRow(
        column(11, htmlOutput("metadata_info_out6"), style = "border: 1px solid grey; margin: 10px; padding: 2px; border-radius: 5px")
      ),
      fluidRow(
        column(11, htmlOutput("metadata_info_out7"), style = "border: 1px solid grey; margin: 10px; padding: 2px; border-radius: 5px")
      )
      
    ))
    output$metadata_info_out1 <- renderUI(display_meta(reactiveValuesToList(metadata), 1))
    output$metadata_info_out2 <- renderUI(display_meta(reactiveValuesToList(metadata), 2))
    output$metadata_info_out3 <- renderUI(display_meta(reactiveValuesToList(metadata), 3))
    output$metadata_info_out4 <- renderUI(display_meta(reactiveValuesToList(metadata), 4))
    output$metadata_info_out5 <- renderUI(display_meta(reactiveValuesToList(metadata), 5))
    output$metadata_info_out6 <- renderUI(display_meta(reactiveValuesToList(metadata), 6))
    output$metadata_info_out7 <- renderUI(display_meta(reactiveValuesToList(metadata), 7))
  })
  
  # simple debuggging/diagnostics elements
  output$debug_adlib_aggression <- renderTable(adlib_agg$dyadic)
  output$rsession_info <- renderPrint(sessionInfo())
  output$current_wd <- renderPrint(getwd())

  observeEvent(input$open_data_dir_abtn, {
    if (dir.exists(metadata$data_root_dir)) system2("open", args = shQuote(metadata$data_root_dir))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
