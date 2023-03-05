read_session_log_files <- function(collection_paths, in_shiny = isRunning()) {
  # define function for single collection
  foo <- function(path) {
    out <- NULL
    xfiles <- list.files(path, full.names = TRUE)
    single_sessionlog <- xfiles[grep("0_log.csv$", xfiles)]
    if (length(single_sessionlog) == 1) {
      out <- read.csv(single_sessionlog)
      if (nrow(out) > 0) {
        out <- cbind(collection_id = basename(gsub(pattern = "0_log.csv$", "", single_sessionlog)), date = NA, out)
        out$date <- as.Date(substr(out$collection_id, 1, 10))
        out$path_foc_nn <- NULL
        out$path_foc_tab <- NULL
        out$path_foc_groom <- NULL
        out$path_foc_aggr <- NULL
      }
    }
    out
  }
  
  # generate output list
  res <- list()
  
  if (in_shiny) {
    withProgress(
      min = 0,
      max = length(collection_paths),
      message = "reading session logs (NEW)",
      {
        for (i in seq_len(length(collection_paths))) {
          res[[length(res) + 1]] <- foo(collection_paths[i])
          incProgress(1)
          Sys.sleep(0.01)
        }
      }
    )
  } else {
    for (i in seq_len(length(collection_paths))) {
      res[[length(res) + 1]] <- foo(collection_paths[i])
    }
  }
  
  # print(search())
  if (length(res) > 1) res <- do.call("rbind", res)
  res
}