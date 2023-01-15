# functions to generate empty tables

# target: daily_sessions$sessions_over_day
# tracks focal sessions within a day
empty_log <- function() {
  data.frame(session = character(0),
             filename = character(0),
             focal_id = character(0),
             focal_counter = integer(0),
             stringsAsFactors = FALSE)
}

