focal_start_session_dialog <- function(potential_focals = NULL) {
  modalDialog(
    span('Start a new focal session.'),
    # if (failed)
    #   div(tags$b("Invalid name of data object", style = "color: red;")),

    selectInput('focal_name', "focal subject", choices = c("_none", potential_focals), selected = "_none"),
    textInput('focal_start', 'focal start time', value = Sys.time() + 60),
    numericInput('focal_duration', 'focal duration', value = 6, min = 2, max = 10, step = 2),

    footer = tagList(
      actionButton("focal_session_start", "START", icon = icon("rocket"), style = "background: rgba(0, 255, 0, 0.5); height:100px"),
      modalButton("Cancel", icon = icon("ban"))
    )
  )
}
