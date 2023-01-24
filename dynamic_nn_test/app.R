
library(shiny)
library(rhandsontable)

source("../helpers/html_styles.R")
source("../helpers/id_table.R")
source("../helpers/render_nn.R")
source("../helpers/helpers.R")



# individual table
all_individuals <- read.csv("../id_table.csv", stringsAsFactors = FALSE)
all_individuals$present <- FALSE
all_individuals$swelling <- factor(NA, levels = c("", 0, 1, 2, 3))
all_individuals$comment <- ""
all_individuals <- all_individuals[all_individuals$id %in% c("as", "cs", "ds", "zj", "cp", "dp", "rm"), ]
group <- "r1"
group_added <- "pb"



# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$style(html_boxes(stylename = "bg-f")), tags$style(html_boxes(stylename = "bg-fsel")), # styles for nn-checkboxes
  tags$style(html_boxes(stylename = "bg-m")), tags$style(html_boxes(stylename = "bg-msel")),
  tags$style(html_boxes(stylename = "bg-o")), tags$style(html_boxes(stylename = "bg-osel")),
  navbarPage("give me data collada", id = "nav_home",
             tabPanel("home",

             column(10, "",
                    # HTML('<p style= "color: red">bla</p>'),
                    actionButton("start_day", "start day here"),
                    actionButton(inputId = "focal_session_start_abtn", label = "start focal session really "),
                    h3("adlib events, IGE, etc"),
                    actionButton(inputId = "adlib_aggression_dialog", label = "dyadic aggression", style = "background: rgba(0, 0, 0, 0.5)"),
                    actionButton("test_stuff", "test stuff (print to console for debugging)")

             )),
          tabPanel("census/nn",
            column(5, rHandsontableOutput("census_table"), actionButton("addnewrowtocensus", "add new row")),
            column(7, actionButton("submit_nn", "submit scan"), p(), fluidRow(htmlOutput("nn_fem")), p(), fluidRow(htmlOutput("nn_male")), p(), fluidRow(htmlOutput("nn_other"))),
            actionButton("print_grabber", "show grabber"),
            verbatimTextOutput("out_print_grabber")
            )

    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  census <- reactiveValues(census = NULL)
  nn_data <- reactiveValues(nn_data = NULL)
  metadata <- reactiveValues(date = NULL,
                               observer = NULL,
                               group = NULL,
                               focal_sessions_so_far = 0,
                               focal_start = NA, focal_duration = NA, focal_id = NA,
                               get_started = FALSE,
                               session_is_active = FALSE,
                               current_foc_session_id = NA
                             )

  observeEvent(input$start_day, {
    metadata$get_started <- TRUE
    metadata$group <- group
    # initiate census table
    census$census <- id_table_initiate(all_individuals, group = metadata$group, include_nn_ids = FALSE)
  })

  observeEvent(input$focal_session_start_abtn, {
    metadata$session_is_active <- TRUE
    # initiate nn scan table
    nn_data$nn_data <- id_table_initiate(all_individuals, group = metadata$group, n_age_sex_classes = 1, include_nn_ids = TRUE)
    nn_data$nn_data <- ammend_nn_from_census(nn = nn_data$nn_data, census = hot_to_r(input$census_table))
    # cat_table(nn_data$nn_data, head = FALSE)
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
      lapply(isolate(nn_data$nn_data$id), function(X) {
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


  output$census_table <- renderRHandsontable({
    if (!is.null(census$census) & metadata$get_started == TRUE) {
      xtab <- rhandsontable(census$census, rowHeaders = NULL)
      hot_table(xtab, highlightCol = TRUE, highlightRow = TRUE)
    }
  })

  observeEvent(input$census_table, {
    xxx <- hot_to_r(input$census_table)
    # cat("------census:-----------\n")
    # cat_table(xxx, head = FALSE)
    # write.table(xxx, file = paths_day$daily_census, sep = ",", row.names = FALSE, quote = FALSE, dec = ".")
    # update nn table if necessary
    nn_data$nn_data <- ammend_nn_from_census(nn = nn_data$nn_data, census = xxx)
    # cat("--------nn:---------\n")
    # if (!is.null(nn_data$nn_data)) cat_table(nn_data$nn_data, head = FALSE)
  })

  observeEvent(input$addnewrowtocensus, {
    if (!is.null(census$census) & metadata$get_started == TRUE) {
      census$census <- hot_to_r(input$census_table)
      census$census <- rbind(census$census, NA)
    }
  })



}

# Run the application
shinyApp(ui = ui, server = server)
