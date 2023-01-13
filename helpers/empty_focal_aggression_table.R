empty_focal_aggression_table <- function() {
  # for dyadic aggression

  # columns needed:
  # date/time
  # id1
  # id2
  # intensity
  # data.frame(matrix(ncol = 4, nrow = 0, dimnames = list(NULL, c("time_stamp", "id1", "id2", "highest_intensity"))))
  data.frame("time_stamp" = character(1), "focal" = character(1), "id2" = character(1), "highest_intensity" = character(1), "focal_won" = FALSE)
}
