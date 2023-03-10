
reload_day_dialog_box <- function() {
  modalDialog(title = "so you want to reload or continue?",
                        span("please provide the necessary information"),
                        hr(),
                        selectInput("available_days_selector_new", label = "select day and observer", choices = NULL),
                        HTML("<p>if this list is empty: there are no data available to reload</p><p>In this case: either start a new collection, or load an example set...</p>"),
                        actionButton("copy_examples_abtn", "make examples available"),
                        footer = tagList(
                          # modalButton("Cancel"),
                          actionButton("reload_day_doit_abtn", "reload or continue"),
                          actionButton("reload_day_cancel_abtn", "cancel", icon = icon("ban")),
                          
                          # actionButton("startnewday_ok_abtn", "OK", style = "background: rgba(0, 255, 0, 0.5); height:100px; width:100px"),
                          HTML("<p style='color:Khaki;'>to be done: 'are you sure?'-button")
                        )
  )
}



# day_folder="2023-01-16_jeanne"
# basefolder = "www"
# basefolder = "www/2023-02-07_joan"
# basefolder = normalizePath("~/Desktop/data_collector_data")

reload_list_days <- function(basefolder) {
  x <- data.frame(day_folder_display = list.files(basefolder), day_folder_path = list.files(basefolder, full.names = TRUE))
  x$empty <- sapply(x$day_folder_path, function(X)length(list.files(X, pattern = ".csv")) == 0)
  x
}

# make_file_paths <- function(basefolder, metadata) {
#   m <- reactiveValuesToList(metadata)
#   out <- list()
#   out$daily_census <- file.path(basefolder, paste0(metadata$date, "_global_", metadata$observer, "_0_census.csv"))
#   out$sessions_log <- file.path(basefolder, paste0(metadata$date, "_global_", metadata$observer, "_0_log.csv"))
#   out$adlib_aggr <- file.path(basefolder, paste0(metadata$date, "_global_", metadata$observer, "_0_aggr.csv"))
#   out
# }

# reload_day_prep <- function(day_folder, basefolder = "www") {
#   out <- list(sessions_log = NULL, daily_census = NULL, adlib_aggr = NULL)
#   # recreate global filepaths
#   elements <- unlist(strsplit(day_folder, "_"))
#   fps <- list(dirpath = file.path(basefolder, paste0(elements[1], "_", elements[2])))
#   fps$daily_census <- file.path(fps$dirpath, paste0(elements[1], "_global_", elements[2], "_0_census", ".csv"))
#   fps$sessions_log <- file.path(fps$dirpath, paste0(elements[1], "_global_", elements[2], "_0_log", ".csv"))
#   fps$adlib_aggr <- file.path(fps$dirpath, paste0(elements[1], "_global_", elements[2], "_0_aggr", ".csv"))
# 
#   out$fps <- fps
# 
#   # read actual files
#   if (file.exists(fps$sessions_log)) {
#     out$sessions_log <- read.csv(fps$sessions_log)
#   }
#   if (file.exists(fps$daily_census)) {
#     out$daily_census <- read.csv(fps$daily_census)
#   }
#   if (file.exists(fps$adlib_aggr)) {
#     out$adlib_aggr <- read.csv(fps$adlib_aggr)
#   }
# 
#   out
# }

reload_meta <- function(metadata, newmeta) {
  # newmeta is the one-column data frame that represents the metadata: rownames in newmeta correspond to the list names in metadata
  # important: match data types
  
  # check whether names match
  if (!all(names(metadata) %in% rownames(newmeta))) stop("name mismatch in metadata object during reloading... (1)")
  if (!all(rownames(newmeta) %in% names(metadata))) stop("name mismatch in metadata object during reloading... (2)")
  if (!all(names(empty_metadata(as_pure_list = TRUE)) %in% names(metadata))) stop("name mismatch in metadata object during reloading... (3)")
  
  x <- newmeta
  
  metadata
  # other setup variables (once set at beginning of collection)
  metadata$setup_hidecolumns <- as.logical(x["setup_hidecolumns", 1])
  metadata$setup_desktopdir <- as.logical(x["setup_desktopdir", 1])
  metadata$setup_focal_duration_default <- as.numeric(x["setup_focal_duration_default", 1])
  metadata$setup_focal_max_consecutive_oos <- as.numeric(x["setup_focal_max_consecutive_oos", 1])
  
  
  # paths to daily info
  metadata$data_root_dir <- x["data_root_dir", 1]
  metadata$day_dir <- x["day_dir", 1]
  metadata$daily_census <- x["daily_census", 1]
  metadata$daily_census_additional <- x["daily_census_additional", 1]
  metadata$adlib_aggr <- x["adlib_aggr", 1]
  metadata$sessions_log <- x["sessions_log", 1]
  metadata$day_meta <- x["day_meta", 1]

  # paths to active session if any
  metadata$active_foc_tab <- x["active_foc_tab", 1]
  metadata$active_foc_nn <- x["active_foc_nn", 1]
  metadata$active_foc_groom <- x["active_foc_groom", 1]
  metadata$active_foc_aggr <- x["active_foc_aggr", 1]

  # actual meta data for day
  metadata$date <- x["date", 1]
  metadata$observer <- x["observer", 1]
  metadata$group <- x["group", 1]
  metadata$get_started <- as.logical(x["get_started", 1])
  metadata$focal_sessions_so_far <- as.numeric(x["focal_sessions_so_far", 1])
  # current focal session
  metadata$focal_duration <- as.numeric(x["focal_duration", 1])
  metadata$focal_id <- x["focal_id", 1]
  metadata$focal_start <- x["focal_start", 1]
  metadata$focal_start_hour <- as.numeric(x["focal_start_hour", 1])
  metadata$focal_start_minute <- as.numeric(x["focal_start_minute", 1])
  metadata$session_is_active <- as.logical(x["session_is_active", 1])
  metadata$current_foc_session_id <- x["current_foc_session_id", 1]
  metadata$session_limit_reached <- as.logical(x["session_limit_reached", 1])
  
  # progress within the current focal session
  metadata$progr_target <- as.numeric(x["progr_target", 1])
  metadata$progr_table_lines <- as.numeric(x["progr_table_lines", 1])
  metadata$progr_na_vals <- as.numeric(x["progr_na_vals", 1])
  metadata$progr_oos <- as.numeric(x["progr_oos", 1])
  metadata$progr_act <- as.numeric(x["progr_act", 1])
  metadata$nn_scan_no <- as.numeric(x["nn_scan_no", 1])
  # grooming monitor NEW
  metadata$groom1_in_progress <- as.logical(x["groom1_in_progress", 1])
  metadata$groom1_time_stamp <- x["groom1_time_stamp", 1]
  metadata$groom1_partner <- x["groom1_partner", 1]
  metadata$groom1_session_num <- as.numeric(x["groom1_session_num", 1])
  metadata$groom1_event_num <- as.numeric(x["groom1_event_num", 1])
  metadata$groom1_direction <- as.numeric(x["groom1_direction", 1])
  metadata$groom1_approach_by_focal <- x["groom1_approach_by_focal", 1]
  metadata$groom1_initated_by_focal <- x["groom1_initated_by_focal", 1]
  metadata$groom1_leave_by_focal <- x["groom1_leave_by_focal", 1]
  
  metadata$groom2_in_progress <- as.logical(x["groom2_in_progress", 1])
  metadata$groom2_time_stamp <- x["groom2_time_stamp", 1]
  metadata$groom2_partner <- x["groom2_partner", 1]
  metadata$groom2_session_num <- as.numeric(x["groom2_session_num", 1])
  metadata$groom2_event_num <- as.numeric(x["groom2_event_num", 1])
  metadata$groom2_direction <- as.numeric(x["groom2_direction", 1])
  metadata$groom2_approach_by_focal <- x["groom2_approach_by_focal", 1]
  metadata$groom2_initated_by_focal <- x["groom2_initated_by_focal", 1]
  metadata$groom2_leave_by_focal <- x["groom2_leave_by_focal", 1]
  
  
  
  # editing monitor
  metadata$edit_adlib_aggr <- as.numeric(x["edit_adlib_aggr", 1])
  metadata$edit_focal_grooming <- as.numeric(x["edit_focal_grooming", 1])
  metadata$edit_focal_aggression <- as.numeric(x["edit_focal_aggression", 1])
  
  metadata
}


