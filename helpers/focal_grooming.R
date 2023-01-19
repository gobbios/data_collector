
empty_grooming <- function() {
  data.frame(time_stamp = character(), # as.POSIXlt(strptime("2000-02-29", format = "%F")),
             session = character(),
             withinsession_num = integer(),
             withinevent_num = integer(),
             focal = character(),
             partner = character(),
             direction = character(),
             approach_by_focal = logical(),
             initated_by_focal = logical(),
             leave_by = character())
}




grooming_table_update <- function(grooming, event = c("start", "change", "end"), input_list, metadata_list) {
  if (event == "start") {
    newrow <- nrow(grooming) + 1
    grooming[newrow, ] <- NA
    grooming$withinevent_num[newrow] <- metadata_list$grooming_withinevent_num
    grooming$time_stamp[newrow] <- as.character(Sys.time())
    grooming$session[newrow] <- metadata_list$current_foc_session_id
    grooming$focal[newrow] <- metadata_list$focal_id
    grooming$partner[newrow] <- metadata_list$grooming_current_parter
    grooming$withinsession_num[newrow] <- metadata_list$grooming_withinsession_num
    grooming$direction[newrow] <- metadata_list$grooming_direction
    grooming$approach_by_focal[newrow] <- input_list$grooming_focal_approach
    grooming$initated_by_focal[newrow] <- input_list$grooming_focal_init
    return(grooming)
  }

  if (event == "change") {
    newrow <- nrow(grooming) + 1
    grooming[newrow, ] <- NA
    grooming$withinevent_num[newrow] <- metadata_list$grooming_withinevent_num
    grooming$time_stamp[newrow] <- as.character(Sys.time())
    grooming$session[newrow] <- metadata_list$current_foc_session_id
    grooming$focal[newrow] <- metadata_list$focal_id
    grooming$partner[newrow] <- metadata_list$grooming_current_parter
    grooming$withinsession_num[newrow] <- metadata_list$grooming_withinsession_num
    grooming$direction[newrow] <- metadata_list$grooming_direction
    grooming$approach_by_focal[newrow] <- input_list$grooming_focal_approach
    grooming$initated_by_focal[newrow] <- input_list$grooming_focal_init
    return(grooming)
  }

  if (event == "end") {
    newrow <- nrow(grooming) + 1
    grooming[newrow, ] <- NA
    grooming$withinevent_num[newrow] <- metadata_list$grooming_withinevent_num
    grooming$time_stamp[newrow] <- as.character(Sys.time())
    grooming$session[newrow] <- metadata_list$current_foc_session_id
    grooming$focal[newrow] <- metadata_list$focal_id
    grooming$partner[newrow] <- metadata_list$grooming_current_parter
    grooming$withinsession_num[newrow] <- metadata_list$grooming_withinsession_num
    grooming$direction[newrow] <- "end"
    grooming$approach_by_focal[newrow] <- input_list$grooming_focal_approach
    grooming$initated_by_focal[newrow] <- input_list$grooming_focal_init
    grooming$leave_by[grooming$withinsession_num == metadata_list$grooming_withinsession_num] <- input_list$grooming_focal_leave
    return(grooming)
  }

}




# prepare textual output for grooming directional message (A grooms B, or B and C groom each other...)
grooming_textual_message <- function(direction, focal_id, current_grooming_parter) {
  out <- NULL
  if (is.na(direction)) return(out)
  if (is.null(direction)) return(out)
  if (direction == "gives") {
    out <- c("grooming is in progress: ", focal_id, "grooms", current_grooming_parter)
  }
  if (direction == "receives") {
    out <- c("grooming is in progress: ", current_grooming_parter, "grooms", focal_id)
  }
  if (direction == "mutual") {
    out <- c("grooming is in progress: ", focal_id, "and", current_grooming_parter, "groom each other")
  }
  out
}
