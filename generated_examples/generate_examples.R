
ids <- c("ap", "bp", "cp", "dp", "ep", "fp", "gp")
observers <- c("petterson", "findus", "maria")
dur <- 74


xdata <- data.frame(date = as.Date("2000-03-31") + seq_len(dur + 1), observer = sample(observers, dur + 1, TRUE))

i = 1
for (i in ids) {
  res <- integer(dur + 1)
  res[1] <- sample(c(0, 1, 2, 3), 1, prob = c(0.6, 0.2, 0.1, 0.1))
  for (d in seq_len(dur)) {
    if (res[d] == 0) res[d + 1] <- sample(c(0, 1, 2, 3), 1, prob = c(0.88, 0.1, 0.01, 0.01))
    if (res[d] == 1) res[d + 1] <- sample(c(0, 1, 2, 3), 1, prob = c(0.01, 0.6, 0.35, 0.01))
    if (res[d] == 2) res[d + 1] <- sample(c(0, 1, 2, 3), 1, prob = c(0.01, 0.01, 0.6, 0.35))
    if (res[d] == 3) res[d + 1] <- sample(c(0, 1, 2, 3), 1, prob = c(0.6, 0.01, 0.01, 0.35))
  }
  
  xdata <- cbind(xdata, res)
  colnames(xdata)[ncol(xdata)] <- i
}


library(shiny)
source("helpers/make_empty_objects.R")


i=1
for (i in 1:nrow(xdata)) {
  basefolder <- file.path("www", paste0(as.character(xdata$date[i]), "_pb_", xdata$observer[i]))
  # meta filename
  metaf <- file.path(basefolder, paste0(as.character(xdata$date[i]), "_global_", xdata$observer[i], "_0_meta.csv"))
  # census filename
  censusf <- file.path(basefolder, paste0(as.character(xdata$date[i]), "_global_", xdata$observer[i], "_0_census.csv"))
  censusf2 <- file.path(basefolder, paste0(as.character(xdata$date[i]), "_global_", xdata$observer[i], "_0_census_additional.csv"))
  logf <- file.path(basefolder, paste0(as.character(xdata$date[i]), "_global_", xdata$observer[i], "_0_log.csv"))
  aggf <- file.path(basefolder, paste0(as.character(xdata$date[i]), "_global_", xdata$observer[i], "_0_aggr.csv"))
  
  # create objects
  m <- empty_metadata(as_pure_list = TRUE)
  m$observer <- xdata$observer[i]
  m$date <- as.character(xdata$date[i])
  m$group <- "pb"
  m$data_root_dir <- normalizePath("www")
  m$day_dir = NA
  m$daily_census = censusf
  m$daily_census_additional = censusf2
  m$adlib_aggr = aggf
  m$sessions_log = logf
  m$day_meta <- metaf 
  m$get_started <- TRUE
  
  
  cen <- data.frame(id = ids, sex = "f", group = "pb", is_focal = TRUE, present = TRUE, swelling = as.numeric(xdata[i, ids]), comment = "", in_nn_tracker = FALSE)
  cen2 <- data.frame(id = character(), sex = character(), group = character(), is_focal = logical(), present = logical(), swelling = numeric(), comment = character(), in_nn_tracker = logical())
  
  
  
  # folder for examples
  dir.create(file.path("www", basename(basefolder)))
  write.csv(data.frame(val = unlist(m)), file = file.path(metaf), row.names = TRUE, quote = FALSE)
  write.table(cen, file = file.path(censusf), sep = ",", row.names = FALSE, quote = FALSE, dec = ".")
  write.table(cen2, file = file.path(censusf2), sep = ",", row.names = FALSE, quote = FALSE, dec = ".")
  write.table(empty_adlib_aggr(), file = file.path(aggf), sep = ",", row.names = FALSE, quote = FALSE, dec = ".")
  write.table(empty_log(), file = file.path(logf), sep = ",", row.names = FALSE, quote = FALSE, dec = ".")
  
  
}

