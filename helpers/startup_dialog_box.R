startup_dialog_box <- function(pot_observers, pot_groups) {
  showModal(modalDialog(title = "hello there, what's up today?",
                        HTML("<p style='color:dodgerblue;'>please ignore any columns that are not self explaining in the generated tables. These columns will eventually be hidden"),
                        hr(),
                        span("please provide the necessary information"),
                        hr(),
                        dateInput("date", "date"),
                        selectInput("observer", "observer", choices = unique(sample(pot_observers))),
                        # selectInput("group", "group", choices = c(all_individuals$group)),
                        selectInput("group", "group", choices = pot_groups, selected = pot_groups[1]),
                        footer = tagList(
                          # modalButton("Cancel"),
                          actionButton("startnewday_setup", "setup", style = "background: dodgerblue; height:30px; width:50px"),
                          actionButton("reload_day_dialog_btn", "reload or continue"),
                          actionButton("startnewday_ok_abtn", "OK", style = "background: rgba(0, 255, 0, 0.5); height:100px; width:100px"),
                          HTML("<p style='color:Khaki;'>to be done: 'are you sure?'-button")
                        )
  ))
}

startup_setup_box <- function(setuplist) {
  showModal(modalDialog(title = "setup", easyClose = FALSE,
                        "these settings won't have any effect for the moment, but will be implemented progressively",
                        checkboxInput("setup_hidecolumns", "show extra columns", value = setuplist$setup_hidecolumns),
                        checkboxInput("setup_desktopdir", "use 'Desktop/data_collector_data' as data directory", value = setuplist$setup_desktopdir),
                        numericInput("setup_focal_duration_default", "default number of point samples per focal session", min = 1, max = 600, step = 1, value = setuplist$setup_focal_duration_default),
                        numericInput("setup_focal_max_consecutive_oos", "number of consecutive oos sample until focal session abort", min = 1, max = 60, step = 1, value = setuplist$setup_focal_max_consecutive_oos),
                        footer = tagList(
                          actionButton("setup_save", "save"),
                          actionButton("setup_cancel", "cancel")
                        )
  ))
}


