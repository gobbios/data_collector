
# 06:45 -> 06:46
# 14:59 -> 15:00
add_one_minute <- function(x) {
  x <- unlist(strsplit(x, ":"))
  if (x[2] == "59") {
    xm <- 0
    xh <- as.numeric(x[1]) + 1
  } else {
    xm <- as.numeric(x[2]) + 1
    xh <- as.numeric(x[1])
  }

  paste0(sprintf("%02.0f", xh), ":", sprintf("%02.0f", xm))
}

# add_one_minute("12:23")
# add_one_minute("05:23")
# add_one_minute("05:59")
