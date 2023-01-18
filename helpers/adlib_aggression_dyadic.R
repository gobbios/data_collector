adlib_aggression_dyadic_dialog <- function() {
  modalDialog("enter event information",
              textInput('adlib_aggression_dyadic_datetime', "", value = as.character(Sys.time())),
              textInput('adlib_aggression_dyadic_id1', "", placeholder = "monkey 1"),
              textInput('adlib_aggression_dyadic_id2', "", placeholder = "monkey 2"),
              radioButtons("adlib_aggression_dyadic_intensity", "highest intensity", choices = c("unk", "high", "low"), selected = "unk"),
              footer = tagList(
                modalButton("Cancel"),
                actionButton("adlib_aggression", "save event")
              )
  )
}

# default for reactive data is adlib_agg$dyadic
adlib_aggression_dyadic_update <- function(reactive_xdata, input_list) {
  reactive_xdata <- rbind(NA, reactive_xdata)
  reactive_xdata$time_stamp[1] <- input_list$adlib_aggression_dyadic_datetime
  reactive_xdata$id1[1] <- input_list$adlib_aggression_dyadic_id1
  reactive_xdata$id2[1] <- input_list$adlib_aggression_dyadic_id2
  reactive_xdata$highest_intensity[1] <- input_list$adlib_aggression_dyadic_intensity

  reactive_xdata
}