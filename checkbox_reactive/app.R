# create checkboxes that change appearance according to their state

library(shiny)

n <- sample(5:8, 1)
ids <- sample(letters[1:n])
marked <- rep(FALSE, n)
sex <- c(sample(c("f", "m")), sample(c("f", "m"), n - 2, TRUE))


myrender <- function(ids, marked, sex) {
  btn_ids <- paste0("id_", ids)
  xclass <- character(length(ids))
  xclass[sex == "f"] <- c("bg-f", "bg-fsel")[marked[sex == "f"] + 1]
  xclass[sex == "m"] <- c("bg-m", "bg-msel")[marked[sex == "m"] + 1]

  out <- lapply(seq_along(marked), function(X) tagAppendAttributes(checkboxInput(btn_ids[X], ids[X], value = marked[X]), class = xclass[X]))
  out
}


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      actionButton("submit", "submit"),
      verbatimTextOutput("results")
    ),

    mainPanel(
      htmlOutput("nn"),

      tags$style(HTML(".bg-f { background-color: rgba(255, 0, 0, 0.05); padding: 10px; color: black; font-weight: bolder; font-size: large; }")),
      tags$style(HTML(".bg-fsel { background-color: rgba(255, 0, 0, 0.6); padding: 10px; color: white; font-weight: bolder; font-size: large;}")),
      tags$style(HTML(".bg-m { background-color: rgba(0, 0, 255, 0.05); padding: 10px; color: black; font-weight: bolder; font-size: large}")),
      tags$style(HTML(".bg-msel { background-color: rgba(0, 0, 255, 0.6); padding: 10px; color: white; font-weight: bolder; font-size: large}"))
    )
  )
)

server <- function(input, output, session) {
  v <- reactiveValues(ini_state = setNames(rep(FALSE, n), ids), firstrun = TRUE)
  m <- reactive({
    setNames(unlist(lapply(1:n, function(X) {
      input[[paste0("id_", ids[X])]]
    })), ids)
  })
  output$nn <- renderUI({
    # myrender(ids, marked = v$ini_state)
    # HTML(paste( myrender(ids, marked = marked)))
    # HTML(paste(myrender(ids, marked = v$ini_state)[[1]]))

    if (v$firstrun) {
      lapply(myrender(ids, marked = v$ini_state, sex = sex), function(X) HTML(paste(X)))
    } else {
      lapply(myrender(ids, marked = m(), sex = sex), function(X) HTML(paste(X)))
    }
  })

  lapply(ids, function(X) {
    observeEvent(input[[paste0("id_", X)]], {
      if (X == ids[length(ids)]) {
        v$firstrun <- FALSE
        v$ini_state <- m()
      }
    })
  })

  observeEvent(input$submit, {
    x <- m()
    res <- names(x)[x]
    output$results <- renderText(res)
  })
}

shinyApp(ui = ui, server = server)
