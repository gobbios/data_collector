focal_start_session_dialog <- function(potential_focals = NULL) {
  pot <- "_none"
  if (!is.null(potential_focals)) pot <- sample(potential_focals)[1]
  modalDialog(
    span('Start a new focal session.'),
    # if (failed)
    #   div(tags$b("Invalid name of data object", style = "color: red;")),

    selectInput('focal_name', "focal subject", choices = c("_none", potential_focals), selected = pot),
    h5("select the starting time"),
    splitLayout(cellWidths = c("15%", "20%", "15%"), 
                actionButton("decr", "", icon = icon("circle-minus")),
                htmlOutput("focal_session_time_val"),
                actionButton("incr", "", icon = icon("circle-plus"))
    ),
    numericInput('focal_duration', 'focal duration', value = 6, min = 2, max = 10, step = 2),

    footer = tagList(
      actionButton("focal_session_start_abtn", "START", icon = icon("rocket"), style = "background: rgba(0, 255, 0, 0.5); height:100px"),
      modalButton("Cancel", icon = icon("ban"))
    )
  )
}
