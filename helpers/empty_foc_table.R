empty_foc_table <- function(start_time = NULL, duration = 30, id = "---", activity_codes = NULL) {
  focal_tab <- data.frame(time_stamp = NA, time_for_display = NA, id = id, activity = NA, loud_call = FALSE, scratches = integer(duration), sample = 1:duration)
  if (!is.null(activity_codes)) {
    focal_tab$activity <- factor(nrow(focal_tab), levels = activity_codes)
  }
  if (!is.null(start_time)) {
    focal_tab$time_stamp <- seq.POSIXt(start_time, by = 60, length.out = duration)
    focal_tab$time_for_display <- strftime(focal_tab$time_stamp, "%H:%M")
  }
  focal_tab
}
