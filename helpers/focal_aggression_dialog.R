focal_aggression_dialog <- function(focal_id) {
  modalDialog("enter event information",
              textInput('focal_aggression_dyadic_datetime', "", value = as.character(Sys.time())),
              textInput('focal_aggression_dyadic_id1', "focal", value = focal_id, placeholder = "focal"),
              textInput('focal_aggression_dyadic_id2', "", placeholder = "monkey 2"),
              radioButtons("focal_aggression_dyadic_intensity", "highest intensity", choices = c("unk", "high", "low"), selected = "unk"),
              checkboxInput("focal_aggression_dyadic_focal_won", "focal won?", value = FALSE),
              footer = tagList(
                modalButton("Cancel"),
                actionButton("focal_aggression", "save event")
              )
  )
}
