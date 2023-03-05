read_census_files <- function(collection_paths, in_shiny = isRunning()) {
  # define function for single collection
  foo <- function(path) {
    out <- NULL
    xfiles <- list.files(path, full.names = TRUE)
    single_census <- xfiles[grep("_census.csv$", xfiles)]
    if (length(single_census) == 1) {
      out <- read.csv(single_census)
      out <- cbind(collection_id = basename(gsub(pattern = "_0_census.csv$", "", single_census)), date = NA, out)
      out$date <- as.Date(substr(out$collection_id, 1, 10))
      out$in_nn_tracker <- NULL
      out$is_focal <- NULL
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

  # print(search())
  if (length(res) > 1) res <- do.call("rbind", res)
  res
}