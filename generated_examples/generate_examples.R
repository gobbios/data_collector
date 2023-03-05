
ids <- c("ap", "bp", "cp", "dp", "ep", "fp", "gp")
observers <- c("petterson", "findus", "maria", "carel", "jeanne")
dur <- 32
bondmat <- matrix(ncol = length(ids), nrow = length(ids), 0)
bondmat[, ] <- rbeta(length(bondmat), 1, 50)
bondmat <- bondmat + t(bondmat)
diag(bondmat) <- 0
colnames(bondmat) <- rownames(bondmat) <- ids
ind_soc <- rbeta(length(ids), 1, 3)
names(ind_soc) <- ids

# hist(rbeta(10000, 1, 4))



# create swelling states
xdata <- data.frame(date = as.Date("2000-03-31") + seq_len(dur + 1))

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


# create observer schedule
odata <- expand.grid(date = xdata$date, observer = observers)
odata <- odata[sample(seq_len(nrow(odata)), nrow(odata) / 2 ), ]
rownames(odata) <- NULL

library(shiny)
source("helpers/make_empty_objects.R")
source("helpers/focal_groom_new.R")

shiny_census <- list()
focalsessions <- list()

i=1
for (i in 1:nrow(odata)) {
  basefolder <- file.path("www", paste0(as.character(odata$date[i]), "_pb_", odata$observer[i]))
  dir.create(file.path("www", basename(basefolder)))
  # meta filename
  metaf <- file.path(basefolder, paste0(as.character(odata$date[i]), "_global_", odata$observer[i], "_0_meta.csv"))
  # census filename
  censusf <- file.path(basefolder, paste0(as.character(odata$date[i]), "_global_", odata$observer[i], "_0_census.csv"))
  censusf2 <- file.path(basefolder, paste0(as.character(odata$date[i]), "_global_", odata$observer[i], "_0_census_additional.csv"))
  logf <- file.path(basefolder, paste0(as.character(odata$date[i]), "_global_", odata$observer[i], "_0_log.csv"))
  aggf <- file.path(basefolder, paste0(as.character(odata$date[i]), "_global_", odata$observer[i], "_0_aggr.csv"))
  
  # create objects
  m <- empty_metadata(as_pure_list = TRUE)
  m$observer <- as.character(odata$observer[i])
  m$date <- as.character(odata$date[i])
  m$group <- "pb"
  m$data_root_dir <- normalizePath("www")
  m$day_dir = NA
  m$daily_census = censusf
  m$daily_census_additional = censusf2
  m$adlib_aggr = aggf
  m$sessions_log = logf
  m$day_meta <- metaf 
  m$get_started <- TRUE
  
  
  cen <- data.frame(id = ids, sex = "f", group = "pb", is_focal = TRUE, present = TRUE, 
                    swelling = as.numeric(xdata[which(xdata$date == odata$date[i]), ids]), comment = "", in_nn_tracker = FALSE)
  cen2 <- data.frame(id = character(), sex = character(), group = character(), is_focal = logical(), present = logical(),
                     swelling = numeric(), comment = character(), in_nn_tracker = logical())
  # build in mismatches
  # 1) data not written (blanks for non-swelling)
  sel <- sample(which(cen$swelling == 0))
  for (jj in seq_len(length(sel))) {
    if (runif(1) > 0.7) cen$swelling[sel[jj]] <- NA
  }
  # 2) mismatches between observers
  sel <- sample(which(cen$swelling != 0))
  for (jj in seq_len(length(sel))) {
    if (runif(1) > 0.95) {
      # if (sel[jj] == 0) cen$swelling[sel[jj]] <- 1
      if (sel[jj] == 1) cen$swelling[sel[jj]] <- sample(c(0, 2), 1)
      if (sel[jj] == 2) cen$swelling[sel[jj]] <- sample(c(1, 3), 1)
      if (sel[jj] == 3) cen$swelling[sel[jj]] <- 2
    }
  }
  
  # create some focal sessions
  sessions_log <- empty_log()
  n_sess <- sample(c(0, 1, 2, 3, 4, 5), 1, prob = c(0.3, 0.1, 0.3, 0.2, 0.1, 0.1))
  m$focal_sessions_so_far <- n_sess
  if (n_sess > 0) {
    for (k in seq_len(n_sess)) {
      d <- strptime(paste(odata$date[i], "06:00:00"), format = "%Y-%m-%d %H:%M:%S")
      d + 3600 * k + sample(seq(-720, 720, by = 60), 1)
      f <- sample(ids, 1)
      
      newrow <- nrow(sessions_log) +1 
      sessions_log[newrow, ] <- NA
      sessions_log$focal_id[newrow] <- f
      sessions_log$focal_counter[newrow] <- table(factor(sessions_log$focal_id, levels = ids))[f]
      sessions_log$session_id[newrow] <- paste0(odata$date[i], "_", f, "_", odata$observer[i], "_", sessions_log$focal_counter[newrow])
      sessions_log$session_created[newrow] <- as.character(Sys.time())
      
      foctab <- empty_foc_table(start_time = d + 3600 * k + sample(seq(-720, 720, by = 60), 1), duration = 10, id = f)
      foctab$activity <- sample(c("fe/fo", "r", "tr"), nrow(foctab), replace = TRUE)
      grooming <- empty_grooming_new()
      grooming$time_stamp <- as.POSIXct(grooming$time_stamp)
      # does grooming happen?
      if (runif(1) > ind_soc[f]) {
        # with whom
        p <- sample(ids, 1, prob = bondmat[f, ])
        # how long
        dur <- round(runif(1, 5, 300))
        
        grooming[1, ] <- NA
        grooming[2, ] <- NA
        if (runif(1) > 0.7) grooming[3, ] <- NA
        if (runif(1) > 0.7) grooming[nrow(grooming) + 1, ] <- NA
        grooming$session <- sessions_log$session_id[newrow]
        grooming$withinevent_num <- 1
        grooming$withinevent_num <- seq_len(nrow(grooming))
        grooming$focal <- f
        grooming$partner <- p
        grooming$direction <- c(sample(c("gives", "receives", "mutual"), nrow(grooming) - 1, replace = TRUE, prob = c(0.475, 0.475, 0.05)), NA)
        grooming$time_stamp[1] <- strptime(foctab$time_stamp[1], format = "%Y_%m_%d@%H:%M:%S") + round(runif(1, 30, 200))
        grooming$event_type[1] <- "start"
        grooming$time_stamp[nrow(grooming)] <- grooming$time_stamp[1] + dur
        grooming$event_type[nrow(grooming)] <- "end"
        extras <- which(is.na(grooming$time_stamp))
        if (length(extras) > 0) {
          cp <- sort(round(runif(length(extras), 1, dur)))
          grooming$time_stamp[extras] <- grooming$time_stamp[1] + cp
          grooming$event_type[extras] <- "change"
        }
        grooming
        # set corresponding point samples to 'gr'
        t1 <- as.POSIXlt(grooming$time_stamp[1])
        t2 <- as.POSIXlt(grooming$time_stamp[nrow(grooming)])
        if (t1$min != t2$min) {
          pos <- strftime(seq(from = t1 + 60 - t1$sec, 
                              to = t2 - t2$sec, 
                              by = 60), format = "%H:%M")
          foctab$activity[foctab$time_for_display %in% pos] <- "gr"
        }
      }
      
      
      foctab$scratches <- rpois(nrow(foctab), lambda = 0.2)
      
      
      # write focal session
      write.csv(foctab, file = file.path(basefolder, paste0(sessions_log$session_id[newrow], "_foctab.csv")), quote = FALSE, row.names = FALSE)
      write.csv(grooming, file = file.path(basefolder, paste0(sessions_log$session_id[newrow], "_groom.csv")), quote = FALSE, row.names = FALSE)
      
    }
  }
  
  
  
  # folder for examples
  
  write.csv(data.frame(val = unlist(m)), file = file.path(metaf), row.names = TRUE, quote = FALSE)
  write.table(cen, file = file.path(censusf), sep = ",", row.names = FALSE, quote = FALSE, dec = ".")
  write.table(cen2, file = file.path(censusf2), sep = ",", row.names = FALSE, quote = FALSE, dec = ".")
  write.table(empty_adlib_aggr(), file = file.path(aggf), sep = ",", row.names = FALSE, quote = FALSE, dec = ".")
  write.table(sessions_log, file = file.path(logf), sep = ",", row.names = FALSE, quote = FALSE, dec = ".")
  
  # and run the code for shiny ------------
  # census sheets
  out <- read.csv(censusf)
  out <- cbind(collection_id = basename(gsub(pattern = "_0_census.csv$", "", censusf)), date = NA, out)
  out$date <- as.Date(substr(out$collection_id, 1, 10))
  shiny_census[[length(shiny_census) + 1]] <- out
  
  # read foctabs
  
  
  

}

shiny_census <- do.call("rbind", shiny_census)
shiny_census$in_nn_tracker <- NULL
censusdata <- shiny_census

swelling_foo(censusdata, "mismatches")

swelling_foo <- function(censusdata, output = c("stages_per_day", "mismatches")) {
  if (output == "mismatches") {
    censusdata$dateid <- paste(censusdata$id, as.character(censusdata$date))
    aux1 <- table(censusdata$dateid)
    if (any(aux1 > 1)) {
      xnames <- names(aux1)[aux1 > 1]
      cdata <- censusdata[censusdata$dateid %in% xnames, ]
      cdata$observer <- unlist(lapply(strsplit(cdata$collection_id, "_"), function(x)rev(x)[1]))
      cdata <- cdata[order(cdata$dateid), ]
      cdata$problem <- FALSE
      for (i in unique(cdata$dateid)) {
        aux2 <- cdata[cdata$dateid == i, ]
        aux3 <- table(aux2$swelling, useNA = "no")
        if (length(aux3) != 1) cdata$problem[cdata$dateid == i] <- TRUE
        if (all(is.na(aux2$swelling))) cdata$problem[cdata$dateid == i] <- FALSE
      }
      cdata <- cdata[cdata$problem, ]
      if (nrow(cdata) > 1) {
        return(cdata[, c("date", "id", "swelling", "observer")])
      } else {
        return(FALSE)
      }
    } else {
      return(FALSE)
    }
  }
  
  
  
  res <- expand.grid(data = seq(min(censusdata$date), max(censusdata$date), by = "day"), id = unique(censusdata$id))
  
  
  
  aggregate(censusdata$swelling, by = list(censusdata$id, censusdata$date), function(x)(table(factor(x, levels = 0:3), useNA = "always")), simplify = FALSE)
  
  x <- tapply(censusdata$swelling, list(date = censusdata$date), function(x)table(factor(x, levels = 0:3), useNA = "always"))
  if (output == "stages_per_day") {
    x <- do.call("rbind", x)
    colnames(x) <- c("swell0", "swell1", "swell2", "swell3", "nodata")
    x <- data.frame(date = as.Date(rownames(x)), x)
    return(x)
  }
}

