library(shiny)

ui <- fluidPage(uiOutput("myiu"), verbatimTextOutput("debugger"))

server <- function(input, output, session) {
  
  n_cases <- 5 # between 1 and 8
  xdata <- list(xdata = data.frame(btn_ids = paste0("btnid_", letters[1:n_cases]), 
                                   id = letters[1:n_cases], 
                                   repl = LETTERS[1:n_cases], 
                                   curr = letters[1:n_cases], 
                                   style = 1))
  # xdata$xdata <- data.frame(rbind(xdata$xdata, xdata$xdata), sex = sample(c("f", "m"), n_cases, replace = TRUE), cat = rep(c(1, 2), each = n_cases))
  xdata <- reactiveValues(xdata = data.frame(btn_ids = paste0("btnid_", letters[1:n_cases]),
                                             id = letters[1:n_cases], 
                                             repl = LETTERS[1:n_cases], 
                                             curr = letters[1:n_cases],
                                             style = 1))
  myfoo <- reactive({
    # generate layout for 2 rows and 3 columns
    pmat <- matrix(ncol = 2, nrow = 3, NA)
    pmat[1:n_cases] <- 1:n_cases
    pmat <- t(pmat)
    
    xxx <- xdata$xdata
    o <- apply(pmat, 1, function(x){
      # print(xdata$xdata$style)
      lapply(x, function(X) {
        if (is.na(X)) return(NULL)
        tagAppendAttributes(style = c("text-align: center; border-radius: 20px; background-color: red", 
                                      "text-align: center; border-radius: 20px; background-color: blue; color: white")[xxx$style[X]],
                            checkboxInput(xxx$btn_ids[X], xxx$curr[X]))
      })
    })
    
    lapply(o, function(x) tagAppendAttributes(class="shiny-split-layout", div(x)))

  })
  

  
  observeEvent(xdata$xdata, once = FALSE, {
    lapply(xdata$xdata$btn_ids, function(oid) {
      observeEvent(input[[oid]], {
        ival <- input[[oid]]
        i <- which(xdata$xdata$btn_ids == oid)
        print(input[[oid]])
        # print(paste("step1", xdata$xdata$curr[i]))
        if (ival) {
          # print(input[[oid]])
          xdata$xdata$curr[i] <- xdata$xdata$repl[i]
          xdata$xdata$style[i] <- 2
          # print(paste("step2", xdata$xdata$curr[i]))
        } else {
        # if (!ival) {
          # print(input[[oid]])
          xdata$xdata$curr[i] <- xdata$xdata$id[i]
          xdata$xdata$style[i] <- 1
        }
        updateCheckboxInput(session, inputId = oid, label = xdata$xdata$curr[i], value = input[[oid]])
      })
    })
  })
  output$myiu <- renderUI(myfoo())

  output$debugger <- renderPrint(xdata$xdata)
}

shinyApp(ui = ui, server = server)
