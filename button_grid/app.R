# create a grid of checkboxes



library(shiny)

# n <- sample(5:8, 1)
n <- 17
ids <- letters[1:n]
marked <- rep(FALSE, n)
sex <- c(sample(c("f", "m")), sample(c("f", "m"), n - 2, TRUE))
do_fems = TRUE

myrender <- function(ids, marked, sex, do_fems = TRUE) {
  btn_ids <- paste0("id_", ids)
  xclass <- character(length(ids))
  xclass[sex == "f"] <- c("bg-f", "bg-fsel")[marked[sex == "f"] + 1]
  xclass[sex == "m"] <- c("bg-m", "bg-msel")[marked[sex == "m"] + 1]
  # xclass <- paste(xclass, "checkbox")
  # out1 <- lapply(which(sex == "f"), function(X) tagAppendAttributes(checkboxInput(btn_ids[X], ids[X], value = marked[X]), class = xclass[X]))
  # out2 <- lapply(which(sex == "m"), function(X) tagAppendAttributes(checkboxInput(btn_ids[X], ids[X], value = marked[X]), class = xclass[X]))
  # c(out1, out2)

  x <- which(sex == "f" | sex == "m")
  if (do_fems) {
    x <- which(sex == "f")
  } else {
    x <- which(sex == "m")
  }
  pmat <- matrix(data = c(x, rep(NA, 6 - length(x) %% 6)), ncol = 6, byrow = TRUE)
  if (length(x) %% 6 == 0) pmat <- matrix(data = x, ncol = 6, byrow = TRUE)


  o <- apply(pmat, 2, function(Y) {
    column(2, lapply(na.omit(Y), function(X) tagAppendAttributes(checkboxInput(btn_ids[X], ids[X], value = marked[X]), class = xclass[X])))
  })

  # tagAppendAttributes(fluidRow(o))
  list(tags$style("input[type='checkbox']{ width: 30px; height: 30px; line-height: 30px;}"), o)

}


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          actionButton("submit", "submit"),
          checkboxInput("test", "lab"),
          verbatimTextOutput("results")
        ),

        mainPanel(
          p(),
          fluidRow(htmlOutput("nn_fem")),
          hr(),
          p(),
          fluidRow(htmlOutput("nn_male")),
          # tags$style(HTML("input[type='checkbox']{ width: 30px; height: 30px; line-height: 30px;}")),
          tags$style(HTML(".bg-f { background-color: rgba(255, 0, 0, 0.05); padding: 10px; color: black; font-weight: bolder; font-size: large; } # input[type='checkbox']{ width: 30px; height: 30px; line-height: 30px;}")),
          tags$style(HTML(".bg-fsel { background-color: rgba(255, 0, 0, 0.6); padding: 10px; color: white; font-weight: bolder; font-size: large;}")),
          tags$style(HTML(".bg-m { background-color: rgba(0, 0, 255, 0.05); padding: 10px; color: black; font-weight: bolder; font-size: large}")),
          tags$style(HTML(".bg-msel { background-color: rgba(0, 0, 255, 0.6); padding: 10px; color: white; font-weight: bolder; font-size: large}"))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  v <- reactiveValues(ini_state = setNames(rep(FALSE, n), ids), firstrun = TRUE)
  # m <- reactive({
  #   setNames(unlist(lapply(1:n, function(X) {
  #     input[[paste0("id_", ids[X])]]
  #   })), ids)
  # })
  output$nn_fem <- renderUI({
    # myrender(ids, marked = v$ini_state)
    # HTML(paste( myrender(ids, marked = marked)))
    # HTML(paste(myrender(ids, marked = v$ini_state)[[1]]))
    lapply(myrender(ids, marked = v$ini_state, sex = sex, do_fems = TRUE), function(X) HTML(paste(X)))
    # if (v$firstrun) {
    #   lapply(myrender(ids, marked = v$ini_state, sex = sex), function(X) HTML(paste(X)))
    # } else {
    #   lapply(myrender(ids, marked = m(), sex = sex), function(X) HTML(paste(X)))
    # }
  })
  output$nn_male <- renderUI({
    lapply(myrender(ids, marked = v$ini_state, sex = sex, do_fems = FALSE), function(X) HTML(paste(X)))
  })

  observeEvent(input$submit, {
    print(cbind(sex, v$ini_state))
  })


}

# Run the application
shinyApp(ui = ui, server = server)


# tag style for larger check boxes...
# tags$style("
#       .checkbox { /* checkbox is a div class*/
#         line-height: 30px;
#         margin-bottom: 10px; /*set the margin, so boxes don't overlap*/
#       }
      # input[type='checkbox']{ /* style for checkboxes */
      #   width: 30px; /*Desired width*/
      #   height: 30px; /*Desired height*/
      #   line-height: 30px;
      # }
#       span {
#           margin-left: 15px;  /*set the margin, so boxes don't overlap labels*/
#           line-height: 30px;
#       }
#   "),

