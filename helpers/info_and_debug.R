
display_meta <- function(x, xdisp) {
  # generate HTML for display of meta data in a modal box
  # display 1: 'real' meta data (day-info) 
  # display 2: current (active) focal session
  # display 3: progress within current focal session
  # display 4: grooming monitor
  # display 5: folders/directories to daily files
  # display 6: paths to files re active focal session (if any)
  # display 7: monitoring which row if any is supposed to be edited in reviewing pane tables
  
  # check whether all elements in meta data are covered (fail-safe if something new is added)
  xnames1 <- c("date", "observer", "group", "get_started", "focal_sessions_so_far")
  xnames2 <- c("focal_duration", "focal_id", "focal_start", "focal_start_hour", "focal_start_minute", "session_is_active", "current_foc_session_id")
  xnames3 <- c("progr_target", "progr_table_lines", "progr_na_vals", "progr_oos", "progr_act", "nn_scan_no")
  xnames4 <- c("grooming_in_progress", "grooming_direction", "grooming_current_parter", "grooming_withinsession_num", "grooming_withinevent_num")
  xnames5 <- c("day_dir", "data_root_dir", "daily_census", "adlib_aggr", "sessions_log", "day_meta")
  xnames6 <- c("active_foc_tab", "active_foc_nn", "active_foc_groom", "active_foc_aggr")
  xnames7 <- c("edit_adlib_aggr")
  
  if(!all(names(x) %in% c(xnames1, xnames2, xnames3, xnames4, xnames5, xnames6, xnames7))) {
    print(names(x))
    warning("there seems to be meta data that is ignored for *display* of meta data")
  }
  
  if (xdisp == 1) {
    out <- paste(paste0("<span style='color:grey'>", xnames1, ":</span> ", x[xnames1]), "<br>")
  }
  if (xdisp == 2) {
    out <- paste(paste0("<span style='color:grey'>", xnames2, ":</span> ", x[xnames2]), "<br>")
  }
  if (xdisp == 3) {
    out <- paste(paste0("<span style='color:grey'>", xnames3, ":</span> ", x[xnames3]), "<br>")
  }
  if (xdisp == 4) {
    out <- paste(paste0("<span style='color:grey'>", xnames4, ":</span> ", x[xnames4]), "<br>")
  }
  if (xdisp == 5) {
    out <- paste(paste0("<span style='color:grey'>", xnames5, ":</span> ", x[xnames5]), "<br>")
  }
  if (xdisp == 6) {
    out <- paste(paste0("<span style='color:grey'>", xnames6, ":</span> ", x[xnames6]), "<br>")
  }
  if (xdisp == 7) {
    out <- paste(paste0("<span style='color:grey'>", xnames7, ":</span> ", x[xnames7]), "<br>")
  }
  
  HTML(out)
}

# print a table to the console (for debugging...)
cat_table <- function(df, head = TRUE) {
  if (head) df <- head(df)
  df <- as.matrix(df)
  df <- rbind(colnames(df), df)
  df <- cbind(c("cn", substr(rownames(df)[-1], 1, 4)), df)
  cat(paste(apply(df, 1, paste0, collapse = "\t"), collapse = "\n"), "\n")
}
