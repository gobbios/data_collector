# functions to generate empty tables and objects

# target: daily_sessions$sessions_over_day
# tracks focal sessions within a day
empty_log <- function() {
  data.frame(session = character(0),
             filename = character(0),
             focal_id = character(0),
             focal_counter = integer(0),
             stringsAsFactors = FALSE)
}

# metadata to recover/reload days
empty_metadata <- function() {
  reactiveValues(date = NULL,
                 observer = NULL,
                 group = NULL,
                 focal_sessions_so_far = 0,
                 focal_start = NA, focal_duration = NA, focal_id = NA,
                 get_started = FALSE,
                 session_is_active = FALSE,
                 current_foc_session_id = NA

                 )
  # metadata$focal_sessions_so_far
}
