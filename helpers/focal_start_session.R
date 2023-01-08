# button to start a focal session
focal_start_session <- function(potential_focals) {
  modalDialog(
    span('Start a new focal session.'),
    footer = tagList(
      modalButton("Cancel"),
      selectInput('focal_name', "focal subject", choices = c("_none", potential_focals), selected = "_none"),
      textInput('focal_start', 'focal start time', value = Sys.time()),
      actionButton("focal_session_start", "START")
    )
  )
}
