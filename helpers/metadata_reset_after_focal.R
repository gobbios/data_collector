
metadata_reset_after_focal <- function(metadata) {
  # this is called when finishing a focal session
  # grooming tracker should be nicely reset already when grooming bout is finished, but it's done here again anyway...
  
  metadata$focal_id <- NA
  metadata$current_foc_session_id <- NA
  metadata$session_is_active <- FALSE
  metadata$focal_start <- NA
  metadata$focal_start_hour <- NA
  metadata$focal_start_minute <- NA
  metadata$focal_duration <- NA
  metadata$progr_target <- NA
  metadata$progr_table_lines <- NA
  metadata$progr_na_vals <- NA
  metadata$progr_oos <- NA
  metadata$progr_act <- NA
  metadata$consecutive_oos <- 0
  metadata$nn_scan_no <- NA
  
  metadata$current_foc_session_id <- NA
  metadata$active_foc_tab <- NA
  metadata$active_foc_nn <- NA
  metadata$active_foc_groom <- NA
  metadata$active_foc_aggr <- NA
  
  metadata$edit_focal_aggression <- NA
  metadata$edit_adlib_aggr <- NA
  metadata$edit_focal_grooming <- NA
  
  # grooming tracker
  metadata$groom1_time_stamp <- NA
  metadata$groom1_session_num <- 0
  metadata$groom1_event_num <- 0
  metadata$groom1_partner <- NA
  metadata$groom1_in_progress <- FALSE
  metadata$groom1_direction <- NA
  metadata$groom1_approach_by_focal <- NA
  metadata$groom1_initated_by_focal <- NA
  metadata$groom1_leave_by_focal <- NA
  metadata$groom2_time_stamp <- NA
  metadata$groom2_session_num <- 0
  metadata$groom2_event_num <- 0
  metadata$groom2_partner <- NA
  metadata$groom2_in_progress <- FALSE
  metadata$groom2_direction <- NA
  metadata$groom2_approach_by_focal <- NA
  metadata$groom2_initated_by_focal <- NA
  metadata$groom2_leave_by_focal <- NA
  
  metadata
}
