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
