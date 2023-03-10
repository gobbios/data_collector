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
                        setup_nn_n_age_sex_classes = 2,
                        setup_nn_buttons_per_row = 6,
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
    out$setup_nn_n_age_sex_classes <- setuplist$setup_nn_n_age_sex_classes
    out$setup_nn_buttons_per_row <- setuplist$setup_nn_buttons_per_row
  }
  
  if (as_pure_list) out <- isolate(reactiveValuesToList(out))
  out
}
