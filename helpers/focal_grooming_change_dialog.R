focal_grooming_change_dialog <- function() {
  modalDialog("enter grooming information",
              column(width = 6,
                     radioButtons("grooming_focal_direction_change", "focal gives or receives?", choices = c("gives", "receives", "mutual"), selected = events_grooming$grooming_direction),
                     actionButton("change_grooming", "roles changed")
              ),
              column(width = 6,
                     radioButtons("grooming_focal_leave", "who leaves?", choices = c("focal", "partner", "unk"), selected = "unk"),
                     actionButton("stop_grooming", "grooming ended")
              ),
              footer = tagList(
                modalButton("Cancel")
              )
  )
}
