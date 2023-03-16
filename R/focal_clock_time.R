#' set focal session start time upon opening the session start dialog and update display
#'
#' @param metadata \code{metadata} reactive object
#' @param jump_value numeric, the seconds to be added
#' @param upordown character: \code{"up"} or \code{"down"}: in- or decrease
#'        time by one minute
#' @aliases update_time_display
#' @return \code{metadata} with updated hour and minute value
#' @export
#'

update_time_display <- function(metadata, upordown) {
  h <- as.numeric(metadata$focal_start_hour)
  m <- as.numeric(metadata$focal_start_minute)
  if (is.na(m) | is.na(h)) stop("focal clock time missing in 'update_time_display'")
  
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
  metadata$focal_start_hour <- h
  metadata$focal_start_minute <- m
  metadata
}

focal_clock_time <- function(metadata, jump_value = 30, printonly = FALSE) {
  x <- Sys.time() + 60
  y <- unlist(as.POSIXlt(x))
  s <- as.numeric(y["sec"])
  if (s >= jump_value) {
    x <- x + 60
    y <- unlist(as.POSIXlt(x))
  }
  if (printonly) {
    return(c(hour = as.numeric(y["hour"]), minute = as.numeric(y["min"])))
    # print(paste( as.numeric(y["hour"]),  ))
    # print(Sys.time())
    # return(NULL)
  } 
  
  metadata$focal_start_hour <- as.numeric(y["hour"])
  metadata$focal_start_minute <- as.numeric(y["min"])
  
  metadata
}

# focal_clock_time(NULL, 30, TRUE)
