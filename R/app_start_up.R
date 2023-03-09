
# update metadata (app setup section) with input from setup dialogue
save_setup <- function(metadata, inputlist) {
  metadata$setup_hidecolumns <- inputlist$setup_hidecolumns
  metadata$setup_desktopdir <- inputlist$setup_desktopdir
  metadata$setup_focal_duration_default <- inputlist$setup_focal_duration_default
  metadata$setup_focal_max_consecutive_oos <- inputlist$setup_focal_max_consecutive_oos
  metadata
}
