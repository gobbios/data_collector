library(shiny)

# initial data
n_cases <- 18 
n_cats <- 2
xdata <- list(xdata = data.frame(btn_ids = paste0("btnid_", letters[1:n_cases]), 
                                 id = letters[1:n_cases], 
                                 # repl = LETTERS[1:n_cases], 
                                 # curr = letters[1:n_cases], 
                                 is_checked = FALSE,
                                 can_be_displayed = TRUE,
                                 style = 1))
if (n_cats == 1) {
  xdata$xdata <- data.frame(xdata$xdata, sex = c(sample(c("f", "m")), sample(c("f", "m"), n_cases - 2, replace = TRUE)), cat = 1)
}
if (n_cats == 2) {
  xdata$xdata <- data.frame(rbind(xdata$xdata, xdata$xdata), sex = c(sample(c("f", "m")), sample(c("f", "m"), n_cases - 2, replace = TRUE)), cat = rep(seq_len(n_cats), each = n_cases))
}
if (n_cats == 3) {
  xdata$xdata <- data.frame(rbind(xdata$xdata, xdata$xdata, xdata$xdata), sex = c(sample(c("f", "m")), sample(c("f", "m"), n_cases - 2, replace = TRUE)), cat = rep(seq_len(n_cats), each = n_cases))
}
xdata$xdata$btn_ids <- paste0(xdata$xdata$btn_ids, "_", xdata$xdata$cat)


# xd=xdata$xdata
# sex="f"
# xcat=1
# max_per_row=9
create_pmat <- function(xd, sex, xcat, max_per_row) {
  x <- which(xd$sex == sex & xd$can_be_displayed & xd$cat == xcat)
  pmat <- matrix(data = c(x, rep(NA, max_per_row - length(x) %% max_per_row)), ncol = max_per_row, byrow = TRUE)
  if (length(x) %% max_per_row == 0) pmat <- matrix(data = x, ncol = max_per_row, byrow = TRUE)
  pmat
}

render_pmat <- function(pmat, xd, sex, xcat, max_per_row) {
  if (nrow(pmat) == 0) return(NULL)
  perc <- paste0(round(95/max_per_row, 2), "%")
  
  # styles
  if (sex == "f") {
    xstyles <- c(paste("width:", perc, "; text-align: center; border-radius: 20px; background-color: rgba(255, 0, 0, 0.05"),
                 paste("width:", perc, "; text-align: center; border-radius: 20px; background-color: rgba(255, 0, 0, 0.6")
    )
  }
  if (sex == "m") {
    xstyles <- c(paste("width:", perc, "; text-align: center; border-radius: 20px; background-color: rgba(0, 0, 255, 0.05"),
                 paste("width:", perc, "; text-align: center; border-radius: 20px; background-color: rgba(0, 0, 255, 0.6")
    )
  }
  
  # xxx <- xd[xd$cat == xcat, ] # filtering not needed...
  xxx <- xd
  o <- apply(pmat, 1, function(x){
    # print(xdata$xdata$style)
    lapply(x, function(X) {
      if (is.na(X)) return(NULL)
      tagAppendAttributes(style = xstyles[xxx$style[X]],
                          checkboxInput(inputId = xxx$btn_ids[X], label = xxx$id[X])) # , xxx$curr[X]
    })
  })
  
  lapply(o, function(x) tagAppendAttributes(class="shiny-split-layout", div(x)))
}

ui <- fluidPage(column(6, 
                       verbatimTextOutput("debugger")
                       ),
                column(6,
                       h4("category 1"),
                       uiOutput("myiu_f1"), uiOutput("myiu_m1"), 
                       h4("category 2"),
                       uiOutput("myiu_f2"), uiOutput("myiu_m2")
                       )
  )

server <- function(input, output, session) {
  xdata <- reactiveValues(xdata = xdata$xdata)
  
  myfoo_f1 <- reactive({
    pmat <- create_pmat(xd = xdata$xdata, sex = "f", xcat = 1, max_per_row = 9)
    render_pmat(pmat, xd = xdata$xdata, sex = "f", xcat = 1, max_per_row = 9)
  })
  myfoo_f2 <- reactive({
    pmat <- create_pmat(xd = xdata$xdata, sex = "f", xcat = 2, max_per_row = 9)
    render_pmat(pmat, xd = xdata$xdata, sex = "f", xcat = 2, max_per_row = 9)
  })
  myfoo_m1 <- reactive({
    pmat <- create_pmat(xd = xdata$xdata, sex = "m", xcat = 1, max_per_row = 9)
    render_pmat(pmat, xd = xdata$xdata, sex = "m", xcat = 1, max_per_row = 9)
  })
  myfoo_m2 <- reactive({
    pmat <- create_pmat(xd = xdata$xdata, sex = "m", xcat = 2, max_per_row = 9)
    render_pmat(pmat, xd = xdata$xdata, sex = "m", xcat = 2, max_per_row = 9)
  })

  
  foo <- function(input, xdata, oid, session) {
    ival <- input[[oid]]
    i <- which(xdata$xdata$btn_ids == oid)
    print(input[[oid]])
    if (ival) {
      xdata$xdata$style[i] <- 2
      xdata$xdata$is_checked[i] <- TRUE
      xdata$xdata$can_be_displayed[which(xdata$xdata$id == xdata$xdata$id[i] & xdata$xdata$btn_ids != xdata$xdata$btn_ids[i])] <- FALSE
    } else {
      xdata$xdata$style[i] <- 1
      xdata$xdata$is_checked[i] <- FALSE
      xdata$xdata$can_be_displayed[which(xdata$xdata$id == xdata$xdata$id[i] & xdata$xdata$btn_ids != xdata$xdata$btn_ids[i])] <- TRUE
    }
    updateCheckboxInput(session, inputId = oid, value = input[[oid]]) # , label = xdata$xdata$curr[i]
    xdata
  }
  

  observeEvent(xdata$xdata, once = FALSE, {
    lapply(xdata$xdata$btn_ids, function(oid) {
      observeEvent(input[[oid]], {
        xdata <- foo(input = input, xdata = xdata, oid = oid, session = session)
      })
    })
  })

  # observeEvent(xdata$xdata, once = FALSE, {
  #   lapply(xdata$xdata$btn_ids, function(oid) {
  #     observeEvent(input[[oid]], {
  #       ival <- input[[oid]]
  #       i <- which(xdata$xdata$btn_ids == oid)
  #       print(input[[oid]])
  #       # print(paste("step1", xdata$xdata$curr[i]))
  #       if (ival) {
  #         # print(input[[oid]])
  #         xdata$xdata$curr[i] <- xdata$xdata$repl[i]
  #         xdata$xdata$style[i] <- 2
  #         xdata$xdata$is_checked[i] <- TRUE
  #         xdata$xdata$can_be_displayed[which(xdata$xdata$id == xdata$xdata$id[i] & xdata$xdata$btn_ids != xdata$xdata$btn_ids[i])] <- FALSE
  #         # print(paste("step2", xdata$xdata$curr[i]))
  #       } else {
  #       # if (!ival) {
  #         # print(input[[oid]])
  #         xdata$xdata$curr[i] <- xdata$xdata$id[i]
  #         xdata$xdata$style[i] <- 1
  #         xdata$xdata$is_checked[i] <- FALSE
  #         xdata$xdata$can_be_displayed[which(xdata$xdata$id == xdata$xdata$id[i] & xdata$xdata$btn_ids != xdata$xdata$btn_ids[i])] <- TRUE
  #         
  #       }
  #       updateCheckboxInput(session, inputId = oid, label = xdata$xdata$curr[i], value = input[[oid]])
  #     })
  #   })
  # })
  
  output$myiu_f1 <- renderUI(myfoo_f1())
  output$myiu_f2 <- renderUI(myfoo_f2())
  output$myiu_m1 <- renderUI(myfoo_m1())
  output$myiu_m2 <- renderUI(myfoo_m2())
  
  output$debugger <- renderPrint(xdata$xdata)
}

shinyApp(ui = ui, server = server)
