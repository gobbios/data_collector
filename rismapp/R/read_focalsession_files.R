read_focalsession_files <- function(collection_paths, in_shiny = isRunning()) {
  # define function for all foctabs in a session
  foo <- function(path) {
    out <- NULL
    xfiles <- list.files(path, full.names = TRUE)
    all_foctabs <- xfiles[grep("_foctab.csv$", xfiles)]
    if (length(all_foctabs) > 0) {
      res <- list()
      for (i in all_foctabs) {
        out <- read.csv(i)
        out <- cbind(collection_id = basename(gsub(pattern = "_0_census.csv$", "", i)), date = NA, out)
        out$date <- as.Date(substr(out$collection_id, 1, 10))
        out$sample <- NULL
        res[[length(res) + 1]] <- out
      }
      out <- do.call("rbind", res)
    }
    out
  }
  
  # generate output list
  res <- list()
  if (in_shiny) {
    withProgress(
      min = 0,
      max = length(collection_paths),
      message = "reading focal sessions (NEW)",
      {
        for (i in seq_len(length(collection_paths))) {
          res[[length(res) + 1]] <- foo(collection_paths[i])
          incProgress(1)
          Sys.sleep(0.005)
        }
      }
    )
  } else {
    for (i in seq_len(length(collection_paths))) {
      res[[length(res) + 1]] <- foo(collection_paths[i])
    }
  }
  
  if (length(res) > 1) res <- do.call("rbind", res)
  res
}