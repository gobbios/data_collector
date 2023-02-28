
# fp_aggr <- "~/Documents/data_collector/www/2023-02-09_pb_julia/2023-02-09_rm_julia_1_aggr.csv"
# fp_groom <- "~/Documents/data_collector/www/2023-02-09_pb_julia/2023-02-09_rm_julia_1_groom.csv"
check_foo <- function(session_id, metadata) {
  out <- list()
  
  # read focal table
  fp_foctab <- file.path(metadata$day_dir, paste0(session_id, "_foctab.csv"))
  foctab <- read.csv(fp_foctab)
  # read grooming table
  fp_groom <- file.path(metadata$day_dir, paste0(session_id, "_groom.csv"))
  groom <- read.csv(fp_groom)
  # read aggression table
  fp_aggr <- file.path(metadata$day_dir, paste0(session_id, "_aggr.csv"))
  aggr <- read.csv(fp_aggr)
  
  aggr$focal <- gsub(" ", "", as.character(aggr$focal))
  aggr$focal[which(aggr$focal == "")] <- NA
  aggr$id2 <- gsub(" ", "", as.character(aggr$id2))
  aggr$id2[which(aggr$id2 == "")] <- NA
  
  print(nrow(aggr))
  if (nrow(aggr) > 0) {
    for (i in seq_len(nrow(aggr))) {
      if (is.na(aggr$focal[i])) out[[length(out) + 1]] <- paste0("at least one interaction partner missing (aggression table at: ", strftime(aggr$time_stamp[i], format = "%T"), ")")
      if (is.na(aggr$id2[i])) out[[length(out) + 1]] <- paste0("at least one interaction partner missing (aggression table at: ", strftime(aggr$time_stamp[i], format = "%T"), ")")
      
      if (!is.na(aggr$focal[i]) & !is.na(aggr$id2[i]) & aggr$focal[i] == aggr$id2[i]) out[[length(out) + 1]] <- paste0(shQuote(aggr$focal[i]), " interacted with itself (aggression table at: ", strftime(aggr$time_stamp[i], format = "%T"), ")")
      
    }
  }
  
  
  out
}



