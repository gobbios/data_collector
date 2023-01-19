focal_aggression_dialog <- function(focal_id) {
  modalDialog("enter event information",
              textInput('focal_aggression_dyadic_datetime', "", value = as.character(Sys.time())),
              textInput('focal_aggression_dyadic_id1', "focal", value = focal_id, placeholder = "focal"),
              textInput('focal_aggression_dyadic_id2', "", placeholder = "monkey 2"),
              radioButtons("focal_aggression_dyadic_intensity", "highest intensity", choices = c("unk", "high", "low"), selected = "unk"),
              checkboxInput("focal_aggression_dyadic_focal_won", "focal won?", value = FALSE),
              footer = tagList(
                modalButton("Cancel"),
                actionButton("focal_aggression_abtn", "save event")
              )
  )
}

# default for reactive data is adlib_agg$dyadic
focal_aggression_dyadic_update <- function(reactive_xdata, input_list) {
  newrow <- nrow(reactive_xdata) + 1
  reactive_xdata[newrow, ] <- NA
  reactive_xdata$time_stamp[newrow] <- input_list$focal_aggression_dyadic_datetime
  reactive_xdata$focal[newrow] <- input_list$focal_aggression_dyadic_id1
  reactive_xdata$id2[newrow] <- input_list$focal_aggression_dyadic_id2
  reactive_xdata$highest_intensity[newrow] <- input_list$focal_aggression_dyadic_intensity
  reactive_xdata$focal_won[newrow] <- input_list$focal_aggression_dyadic_focal_won

  reactive_xdata
}

