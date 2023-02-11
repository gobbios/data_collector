curtime <- function() {
  x <- unlist(as.POSIXlt(Sys.time()))
  h <- as.numeric(x["hour"])
  m <- as.numeric(x["min"])
  s <- ceiling(as.numeric(x["sec"]))
  
  if (s > 30) {
    m <- m + 1
  }
  if (m == 60) {
    m <- 0
    h <- h + 1
  }
  
  c(h, m)
}

update_time_display <- function(metadata, upordown, xtime = curtime()) {
  h <- as.numeric(metadata$focal_start_hour)
  m <- as.numeric(metadata$focal_start_minute)
  if (is.na(m)) m <- xtime[2]
  if (is.na(h)) h <- xtime[1]
  
  if (upordown == "up") {
    m <- m + 1
    if (m == 60) {
      m <- 0
      h <- h + 1
    }
  }
  if (upordown == "down") {
    m <- m - 1
    if (m == -1) {
      m <- 59
      h <- h - 1
    }
  }
  c(h, m)
}


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
