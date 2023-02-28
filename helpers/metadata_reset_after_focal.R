
metadata_reset_after_focal <- function(metadata) {
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
  metadata$grooming_in_progress <- FALSE
  metadata$grooming_direction <- NA
  metadata$grooming_current_parter <- NA
  metadata$grooming_withinsession_num <- 1
  metadata$grooming_withinevent_num <- 1
  
  metadata$current_foc_session_id <- NA
  metadata$active_foc_tab <- NA
  metadata$active_foc_nn <- NA
  metadata$active_foc_groom <- NA
  metadata$active_foc_aggr <- NA
  
  metadata$edit_focal_aggression <- NA
  metadata$edit_adlib_aggr <- NA
  metadata$edit_focal_grooming <- NA
  metadata
}
