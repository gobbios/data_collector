
start_rismapp <- function() {
  # check whether package is there and run from rstudio...
  willwork <- require(rstudioapi, quietly = TRUE)
  if (willwork) willwork <- rstudioapi::isAvailable()
  if (willwork) {
    jobid <- rstudioapi::jobRunScript(path = "helpers/launch_second.R")
    rstudioapi::jobSetState(jobid, "succeeded")
    # rstudioapi::jobRemove(jobid)
  } else {
    showModal(modalDialog(span(p("this won't work unless you are running this app from RStudio AND have the 'rstudioapi' package installed")),
                          span(p("in any case, this is just a shortcut to start another app that is supposed to show the analysis side of things"))))
  }
}

display_meta <- function(x, xdisp) {
  # generate HTML for display of meta data in a modal box
  # display 1: 'real' meta data (day-info) 
  # display 2: current (active) focal session
  # display 3: progress within current focal session
  # display 4: setup values
  # display 5: folders/directories to daily files
  # display 6: paths to files re active focal session (if any)
  # display 7: monitoring which row if any is supposed to be edited in reviewing pane tables
  # display 8: grooming monitor 1
  # display 9: grooming monitor 2
  
  # check whether all elements in meta data are covered (fail-safe if something new is added)
  xnames1 <- c("date", "observer", "group", "get_started", "focal_sessions_so_far")
  xnames2 <- c("focal_duration", "focal_id", "focal_start", "focal_start_hour", 
               "focal_start_minute", 
               "session_is_active", "session_limit_reached", "current_foc_session_id")
  xnames3 <- c("progr_target", "progr_table_lines", "progr_na_vals", "progr_oos", 
               "progr_act", "nn_scan_no", "consecutive_oos")
  xnames4 <- c("setup_hidecolumns", "setup_desktopdir", "setup_focal_duration_default", 
               "setup_focal_max_consecutive_oos", "setup_n_nn_scans",
               "setup_nn_n_age_sex_classes", "setup_nn_buttons_per_row")
  
  xnames5 <- c("day_dir", "data_root_dir", "daily_census", "daily_census_additional", 
               "adlib_aggr", "sessions_log", "day_meta")
  xnames6 <- c("active_foc_tab", "active_foc_nn", "active_foc_groom", "active_foc_aggr")
  xnames7 <- c("edit_adlib_aggr", "edit_focal_grooming", "edit_focal_aggression")
  
  xnames8 <- c("groom1_in_progress", "groom1_direction", "groom1_partner", "groom1_session_num", 
               "groom1_event_num", "groom1_time_stamp", "groom1_approach_by_focal", 
               "groom1_initated_by_focal", "groom1_leave_by_focal")
  xnames9 <- c("groom2_in_progress", "groom2_direction", "groom2_partner", "groom2_session_num", 
               "groom2_event_num", "groom2_time_stamp", "groom2_approach_by_focal",
               "groom2_initated_by_focal", "groom2_leave_by_focal")
  
  if(!all(names(x) %in% c(xnames1, xnames2, xnames3, xnames4, xnames8, xnames9, xnames5, xnames6, xnames7))) {
    warning("there seems to be meta data that is ignored for *display* of meta data (info_and_debug.R)") # print(names(x))
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
  if (xdisp == 8) {
    out <- paste(paste0("<span style='color:grey'>", xnames8, ":</span> ", x[xnames8]), "<br>")
  }
  if (xdisp == 9) {
    out <- paste(paste0("<span style='color:grey'>", xnames9, ":</span> ", x[xnames9]), "<br>")
  }
  
  HTML(out)
}

show_metadata <- function(output, metadata) {
  # m <- isolate(unlist(metadata))
  # print(m[order(names(m))])
  showModal(modalDialog(
    # title = "",
    easyClose = TRUE,
    fluidRow(
      column(5, htmlOutput("metadata_info_out1"), style = "border: 1px solid grey; margin: 10px; padding: 2px; border-radius: 10px"), 
      column(5, htmlOutput("metadata_info_out2"), style = "border: 1px solid grey; margin: 10px; padding: 2px; border-radius: 10px")
    ),
    # hr(),
    fluidRow(
      column(5, htmlOutput("metadata_info_out3"), style = "border: 1px solid grey; margin: 10px; padding: 2px; border-radius: 10px"),
      column(5, htmlOutput("metadata_info_out4"), style = "border: 1px solid grey; margin: 10px; padding: 2px; border-radius: 10px")
    ),
    fluidRow(
      column(5, htmlOutput("metadata_info_out8"), style = "border: 1px solid grey; margin: 10px; padding: 2px; border-radius: 10px"), 
      column(5, htmlOutput("metadata_info_out9"), style = "border: 1px solid grey; margin: 10px; padding: 2px; border-radius: 10px")
    ),
    fluidRow(
      column(11, htmlOutput("metadata_info_out5"), style = "border: 1px solid grey; margin: 10px; padding: 2px; border-radius: 5px")
    ),
    fluidRow(
      column(11, htmlOutput("metadata_info_out6"), style = "border: 1px solid grey; margin: 10px; padding: 2px; border-radius: 5px")
    ),
    fluidRow(
      column(11, htmlOutput("metadata_info_out7"), style = "border: 1px solid grey; margin: 10px; padding: 2px; border-radius: 5px")
    )
    
  ))
  output$metadata_info_out1 <- renderUI(display_meta(reactiveValuesToList(metadata), 1))
  output$metadata_info_out2 <- renderUI(display_meta(reactiveValuesToList(metadata), 2))
  output$metadata_info_out3 <- renderUI(display_meta(reactiveValuesToList(metadata), 3))
  output$metadata_info_out4 <- renderUI(display_meta(reactiveValuesToList(metadata), 4))
  output$metadata_info_out5 <- renderUI(display_meta(reactiveValuesToList(metadata), 5))
  output$metadata_info_out6 <- renderUI(display_meta(reactiveValuesToList(metadata), 6))
  output$metadata_info_out7 <- renderUI(display_meta(reactiveValuesToList(metadata), 7))
  output$metadata_info_out8 <- renderUI(display_meta(reactiveValuesToList(metadata), 8))
  output$metadata_info_out9 <- renderUI(display_meta(reactiveValuesToList(metadata), 9))
}

# print a table to the console (for debugging...)
cat_table <- function(df, head = TRUE) {
  if (head) df <- head(df)
  df <- as.matrix(df)
  df <- rbind(colnames(df), df)
  df <- cbind(c("cn", substr(rownames(df)[-1], 1, 4)), df)
  cat(paste(apply(df, 1, paste0, collapse = "\t"), collapse = "\n"), "\n")
}
