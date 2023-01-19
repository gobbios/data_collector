# for now: binary decision between using directory within the R project or a directory on the Desktop
# creates directory as required (and if on Desktop: display a message)
# returns the path to the folder

link_directory <- function(use_dir_on_desktop) {
  if (use_dir_on_desktop) {
    fp <- normalizePath("~/Desktop/data_collector_data", mustWork = FALSE)
  } else {
    fp <- normalizePath("www", mustWork = FALSE)
    if (!dir.exists(fp)) dir.create(fp)
  }
  if (!dir.exists(fp) & use_dir_on_desktop) {
    dir.create(fp)
    showModal(modalDialog("created directory on Desktop: 'data_collector_data'"))
    Sys.sleep(5)
  }
  fp
}
