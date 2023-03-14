
# update metadata (app setup section) with input from setup dialogue
save_setup <- function(metadata = NULL, inputlist = NULL, return_item_names = FALSE) {
  items <- c("setup_hidecolumns",
             "setup_desktopdir", 
             "setup_focal_duration_default",
             "setup_focal_max_consecutive_oos",
             "setup_n_nn_scans",
             "setup_nn_n_age_sex_classes", 
             "setup_nn_buttons_per_row")
  if (return_item_names) return(items)
  
  for (i in items) metadata[[i]] <- inputlist[[i]]
  metadata
}
