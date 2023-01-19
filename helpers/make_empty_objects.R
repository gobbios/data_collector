# functions to generate empty tables and objects

# tracks focal sessions within a day
# reactive target: daily_sessions$sessions_over_day
empty_log <- function() {
  data.frame(session_id = character(0),
             session_created = character(0),
             filename = character(0),
             focal_id = character(0),
             focal_counter = integer(0),
             stringsAsFactors = FALSE)
}

# metadata to recover/reload days
# reactive target: 'metadata'
empty_metadata <- function() {
  reactiveValues(date = NULL,
                 observer = NULL,
                 group = NULL,
                 focal_sessions_so_far = 0,
                 focal_start = NA, focal_duration = NA, focal_id = NA,
                 get_started = FALSE,
                 session_is_active = FALSE,
                 current_foc_session_id = NA,
                 # progress within the current focal session
                 progr_target = NA,
                 progr_table_lines = NA,
                 progr_na_vals = NA,
                 progr_oos = NA,
                 progr_act = NA,
                 # grooming monitor
                 grooming_in_progress = FALSE,
                 grooming_direction = NA,
                 grooming_current_parter = NA,
                 grooming_withinsession_num = 1,
                 grooming_withinevent_num = 1
                 )
  # metadata$focal_sessions_so_far
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
  data.frame("time_stamp" = character(0), "id1" = character(0), "id2" = character(0), "highest_intensity" = character(0))
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
