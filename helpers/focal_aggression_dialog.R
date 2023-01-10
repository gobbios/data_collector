focal_aggression_dialog <- function(focal_id) {
  modalDialog("enter event information",
              textInput('adlib_aggression_dyadic_datetime', "", value = as.character(Sys.time())),
              textInput('adlib_aggression_dyadic_id1', "focal", value = focal_id, placeholder = "focal"),
              textInput('adlib_aggression_dyadic_id2', "", placeholder = "monkey 2"),
              radioButtons("adlib_aggression_dyadic_intensity", "highest intensity", choices = c("unk", "high", "low"), selected = "unk"),
              footer = tagList(
                modalButton("Cancel"),
                actionButton("focal_aggression", "save event")
              )
  )
}
