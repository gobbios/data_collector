# counts the final oos values in a session activity column
count_final_oos <- function(x) {
  x[which(x == "")] <- NA
  x <- rev(na.omit(x))
  if (all(x == "oos")) return(length(x))
  res <- which(x != "oos")[1] - 1
  if (is.na(res)) res <- 0
  res
}