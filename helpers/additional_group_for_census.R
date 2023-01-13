additional_group_for_census <- function(all_groups, current_group) {
  pot_groups <- all_groups[!all_groups %in% current_group]
  modalDialog("add additional group for census data",
              span("are you sure you want to add more individuals to the census table?"),
              selectInput('add_group_selected', label = "additional group", choices = pot_groups, selected = NULL),
              footer = tagList(
                modalButton("Cancel"),
                actionButton("add_group_selected_submit", label = "add group")
              )
  )
}
