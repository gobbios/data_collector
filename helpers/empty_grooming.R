empty_grooming <- function() {
  data.frame(time_stamp = as.POSIXlt(strptime("2000-02-29", format = "%F")),
             session = NA, withinsession_num = as.integer(-1), withinevent_num = as.integer(-1), focal = NA, partner = NA, direction = NA,
             approach_by_focal = NA, initated_by_focal = NA, leave_by = NA)
}
