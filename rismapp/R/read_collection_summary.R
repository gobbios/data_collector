read_collection_summary <- function(collection_paths, in_shiny = isRunning()) {
  # define function for single collection
  foo <- function(path) {
    out <- NULL
    xfiles <- list.files(path, full.names = TRUE)
    single_meta <- xfiles[grep("_0_meta.csv$", xfiles)]
    if (length(single_meta) == 1) {
      out <- read.csv(single_meta, row.names = 1)
      out <- t(out[c("group", "observer", "date", "focal_sessions_so_far"), , drop = FALSE])
    }
    out
  }
  
  # generate output list
  res <- list()
  
  if (in_shiny) {
    withProgress(
      min = 0,
      max = length(collection_paths),
      message = "reading census data (NEW)",
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
  
  if (length(res) > 1) {
    res <- do.call("rbind", res)
    res <- data.frame(res)
    res$date <- as.Date(res$date)
    res$focal_sessions_so_far <- as.integer(res$focal_sessions_so_far)
    rownames(res) <- NULL
  }
  res
}