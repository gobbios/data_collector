focal_start_session_dialog <- function(potential_focals = NULL) {
  modalDialog(
    span('Start a new focal session.'),
    # if (failed)
    #   div(tags$b("Invalid name of data object", style = "color: red;")),

    selectInput('focal_name', "focal subject", choices = c("_none", potential_focals), selected = "_none"),
    textInput('focal_start', 'focal start time', value = Sys.time()),


    footer = tagList(
      actionButton("focal_session_start", "START", style = "background: rgba(0, 255, 0, 0.5);"),
      modalButton("Cancel", icon = icon("ban"))
    )
  )
}
