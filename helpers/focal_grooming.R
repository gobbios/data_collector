
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
