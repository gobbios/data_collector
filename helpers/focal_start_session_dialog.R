focal_start_session_dialog <- function(potential_focals = NULL) {
  modalDialog(
    span('Start a new focal session.'),
    # if (failed)
    #   div(tags$b("Invalid name of data object", style = "color: red;")),
    
    footer = tagList(
      modalButton("Cancel"),
      selectInput('focal_name', "focal subject", choices = c("_none", potential_focals), selected = "_none"),
      textInput('focal_start', 'focal start time', value = Sys.time()),
      actionButton("focal_session_start", "START")
    )
  )
}