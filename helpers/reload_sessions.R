"2022-12-14_census_joan.csv"
# elements
# [1] "2023-01-14" "ap"         "joan"       "1"



# day_folder="2023-01-16_jeanne"
# # basefolder = "www2"
# basefolder = normalizePath("~/Desktop/data_collector_data")

reload_list_days <- function(basefolder) {
  data.frame(day_folder_display = list.files(basefolder), day_folder_path = list.files(basefolder, full.names = TRUE))
}

reload_day_prep <- function(day_folder, basefolder = "www") {
  out <- list(sessions_log = NULL, daily_census = NULL, adlib_aggr = NULL)
  # recreate global filepaths
  elements <- unlist(strsplit(day_folder, "_"))
  fps <- list(dirpath = file.path(basefolder, paste0(elements[1], "_", elements[2])))
  fps$daily_census <- file.path(fps$dirpath, paste0(elements[1], "_global_", elements[2], "_0_census", ".csv"))
  fps$sessions_log <- file.path(fps$dirpath, paste0(elements[1], "_global_", elements[2], "_0_log", ".csv"))
  fps$adlib_aggr <- file.path(fps$dirpath, paste0(elements[1], "_global_", elements[2], "_0_aggr", ".csv"))

  out$fps <- fps

  # read actual files
  if (file.exists(fps$sessions_log)) {
    out$sessions_log <- read.csv(fps$sessions_log)
  }
  if (file.exists(fps$daily_census)) {
    out$daily_census <- read.csv(fps$daily_census)
  }
  if (file.exists(fps$adlib_aggr)) {
    out$adlib_aggr <- read.csv(fps$adlib_aggr)
  }

  out
}

# fps$current_foc_tab <- file.path(fps$dirpath, paste0(session_id, "_foctab.csv"))
# fps$current_foc_nn <- file.path(fps$dirpath, paste0(session_id, "_nn.csv"))
# fps$current_foc_groom <- file.path(fps$dirpath, paste0(session_id, "_groom.csv"))
# fps$current_foc_aggr <- file.path(fps$dirpath, paste0(session_id, "_aggr.csv"))
# fps$current_foc_session_id <- session_id

# folder <- "~/Desktop/data_collector_data/2023-01-16_jeanne"
reload_all_focal_sessions <- function(folder) {
  xpaths <- list.files(folder, full.names = TRUE, pattern = "foctab.csv$")
  out <- lapply(xpaths, function(x) {
    y <- paste(unlist(strsplit(basename(x), "_"))[1:4], collapse = "_")
    data.frame(session_id = y, read.csv(x))
  })
  do.call("rbind", out)
}

# folder <- "www/2023-01-19_jeanne"
read_meta <- function(folder) {
  xpaths <- list.files(folder, full.names = TRUE, pattern = "meta.csv$")
  x <- read.csv(xpaths, row.names = 1)
  out <- list(group = x["group", 1],
              date = x["date", 1],
              observer = x["observer", 1],
              focal_sessions_so_far = as.numeric(x["focal_sessions_so_far", 1]),
              focal_duration = as.numeric(x["focal_duration", 1]),
              focal_start = as.character(x["focal_start", 1]),
              focal_id = x["focal_id", 1],
              get_started = as.logical(x["get_started", 1]),
              session_is_active = as.logical(x["session_is_active", 1])
              )
  # checking
  # y <- names(isolate(reactiveValuesToList(empty_metadata())))
  out
}

