
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
adlib_aggression_dyadic_update <- function(reactive_xdata, input_list, match_table, what_row = NA) {
  input_tab_matching <- match_table[match_table$context == "adlib_agg", ]
  
  if (is.na(what_row)) { 
    newrow <- nrow(reactive_xdata) + 1
    reactive_xdata[newrow, ] <- NA
    reactive_xdata$sample[newrow] <- newrow
  } else {
    newrow <- what_row
  }
  
  for (i in 1:nrow(input_tab_matching)) {
    reactive_xdata[newrow, input_tab_matching[i, "tabcol"]] <- input_list[[input_tab_matching[i, "inputname"]]]
  }
  
  # reactive_xdata$time_stamp[newrow] <- input_list$adlib_aggression_dyadic_datetime
  # reactive_xdata$id1[newrow] <- input_list$adlib_aggression_dyadic_id1
  # reactive_xdata$id2[newrow] <- input_list$adlib_aggression_dyadic_id2
  # reactive_xdata$highest_intensity[newrow] <- input_list$adlib_aggression_dyadic_intensity

  reactive_xdata
}
