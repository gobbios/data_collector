startup_dialog_box <- function(pot_observers, pot_groups) {
  showModal(modalDialog(title = "hello there, what's up today?",
                        span("please provide the necessary information"),
                        hr(),
                        dateInput("date", "date"),
                        selectInput("observer", "observer", choices = unique(sample(pot_observers))),
                        # selectInput("group", "group", choices = c(all_individuals$group)),
                        selectInput("group", "group", choices = pot_groups, selected = pot_groups[1]),
                        checkboxInput("desktopdir", "use 'Desktop/data_collector_data' as data directory", value = FALSE),
                        footer = tagList(
                          # modalButton("Cancel"),
                          actionButton("startnewday_ok_abtn", "OK", style = "background: rgba(0, 255, 0, 0.5); height:100px; width:100px"),
                          HTML("<p style='color:Khaki;'>to be done: 'are you sure?'-button")
                        )
  ))
}
