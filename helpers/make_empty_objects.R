# functions to generate empty tables and objects

# empty table for focal session point samples
empty_foc_table <- function(start_time = NULL, duration = 30, id = "---", activity_codes = NULL) {
  focal_tab <- data.frame(sample = 1:duration, time_stamp = NA, time_for_display = NA, id = id, activity = NA, loud_call = FALSE, scratches = integer(duration))
  
  if (!is.null(activity_codes)) {
    focal_tab$activity <- factor(nrow(focal_tab), levels = activity_codes)
  }
  if (!is.null(start_time)) {
    if (inherits(start_time, "POSIXt")) start_time <- start_time
    if (inherits(start_time, "character")) start_time <- strptime(start_time, format = "%Y-%m-%d %H:%M:%S")
    focal_tab$time_stamp <- seq.POSIXt(start_time, by = 60, length.out = duration)
    focal_tab$time_for_display <- strftime(focal_tab$time_stamp, "%H:%M")
    focal_tab$time_stamp <- as.character(focal_tab$time_stamp)
    focal_tab$time_stamp <- gsub(" ", "@", focal_tab$time_stamp)
    focal_tab$time_stamp <- gsub("-", "_", focal_tab$time_stamp)
  }
  focal_tab
}


# tracks focal sessions within a day
empty_log <- function() {
  data.frame(session_id = character(0),
             session_created = character(0),
             focal_id = character(0),
             focal_counter = integer(0),
             path_foc_tab = character(0),
             path_foc_nn = character(0),
             path_foc_groom = character(0),
             path_foc_aggr = character(0),
             stringsAsFactors = FALSE)
}




# metadata to recover/reload days
empty_metadata <- function(as_pure_list = FALSE, setuplist = NULL) {
  out <- reactiveValues(date = NULL,
                        observer = NULL,
                        group = NULL,
                        get_started = FALSE,
                        focal_sessions_so_far = 0,
                        # other setup variables (once set at beginning of collection)
                        setup_hidecolumns = FALSE,
                        setup_desktopdir = FALSE,
                        setup_focal_duration_default = 6,
                        setup_focal_max_consecutive_oos = 5,
                        # paths to active focal session files, if any
                        active_foc_tab = NA, 
                        active_foc_nn = NA, 
                        active_foc_groom = NA, 
                        active_foc_aggr = NA,
                        # paths to folders/directories
                        data_root_dir = NULL,
                        day_dir = NULL,
                        daily_census = NULL,
                        daily_census_additional = NULL,
                        adlib_aggr = NULL,
                        sessions_log = NULL,
                        day_meta = NULL,
                        # current focal session
                        focal_duration = NA, 
                        focal_id = NA,
                        focal_start = NA,
                        focal_start_hour = NA, 
                        focal_start_minute = NA,
                        session_is_active = FALSE,
                        session_limit_reached = FALSE,
                        current_foc_session_id = NA,
                        # progress within the current focal session
                        progr_target = NA,
                        progr_table_lines = NA,
                        progr_na_vals = NA,
                        progr_oos = NA,
                        progr_act = NA,
                        consecutive_oos = 0,
                        nn_scan_no = 0, # for nn scans...
                        # grooming new
                        groom1_time_stamp = NA,
                        groom1_session_num = 0,
                        groom1_event_num = 0,
                        groom1_partner = NA,
                        groom1_in_progress = FALSE,
                        groom1_direction = NA,
                        groom1_approach_by_focal = NA,
                        groom1_initated_by_focal = NA,
                        groom1_leave_by_focal = NA,
                        groom2_time_stamp = NA,
                        groom2_session_num = 0,
                        groom2_event_num = 0,
                        groom2_partner = NA,
                        groom2_in_progress = FALSE,
                        groom2_direction = NA,
                        groom2_approach_by_focal = NA,
                        groom2_initated_by_focal = NA,
                        groom2_leave_by_focal = NA,
                        
                        # editing monitor for reviewing pane
                        edit_adlib_aggr = NA,
                        edit_focal_grooming = NA,
                        edit_focal_aggression = NA
  )
  
  if (!is.null(setuplist)) {
    out$setup_hidecolumns <- setuplist$setup_hidecolumns
    out$setup_desktopdir <- setuplist$setup_desktopdir
    out$setup_focal_duration_default <- setuplist$setup_focal_duration_default
    out$setup_focal_max_consecutive_oos <- setuplist$setup_focal_max_consecutive_oos
  }
  
  if (as_pure_list) out <- isolate(reactiveValuesToList(out))
  out
}




# adlib aggression
# reactive target: 'adlib_agg$dyadic'
empty_adlib_aggr <- function() {
  # for dyadic aggression

  # columns needed:
  # date/time
  # id1
  # id2
  # intensity
  data.frame("sample" = integer(0), "time_stamp" = character(0), "id1" = character(0), "id2" = character(0), "highest_intensity" = character(0))
}

# focal aggression
empty_focal_aggr <- function() {
  # for dyadic aggression

  # columns needed:
  # date/time
  # id1
  # id2
  # intensity
  data.frame("time_stamp" = character(0), "focal" = character(0), "id2" = character(0), "highest_intensity" = character(0), "focal_won" = logical(0))
}


# storage container for nn data per session
empty_nn_storage <- function() {
  data.frame(session_id = character(), scan_no = integer(), id = character(), present = logical())
}
