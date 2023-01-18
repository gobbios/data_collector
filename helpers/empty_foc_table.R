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
  }
  focal_tab
}
