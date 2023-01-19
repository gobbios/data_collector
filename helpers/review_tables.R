review_table_foctab <- function(input, paths_day, metadata) {
  sess <- gsub(".*\\((.*)\\).*", "\\1", input$session_for_review)
  fp <- file.path(paths_day$dirpath, paste0(sess, "_foctab.csv"))
  if (metadata$focal_sessions_so_far > 0 & file.exists(fp)) {
    # outtab <- read.csv(paste0("www/", sess, ".csv"))
    outtab <- read.csv(fp)
    # print(fp)
    outtab <- rhandsontable(outtab, rowHeaders = NULL, height = 500)
    # outtab <- hot_col(outtab, "scratches", readOnly = TRUE)
    # hot_table(outtab, highlightCol = TRUE, highlightRow = TRUE)
    outtab <- hot_context_menu(outtab, allowRowEdit = FALSE, allowColEdit = FALSE)
    return(outtab)
  }
  NULL
}


review_table_nn <- function(input, paths_day, metadata) {
  sess <- gsub(".*\\((.*)\\).*", "\\1", input$session_for_review)
  fp <- file.path(paths_day$dirpath, paste0(sess, "_nn.csv"))
  if (metadata$focal_sessions_so_far > 0 & file.exists(fp)) {
    if (!isTRUE(readLines(fp) == "")) {
      outtab <- read.csv(fp)
      colnames(outtab) <- c("id", paste0("scan", seq_len(ncol(outtab) - 1)))
      # print(head(outtab))
      outtab <- rhandsontable(outtab, rowHeaders = NULL, height = 500)
      # outtab <- hot_col(outtab, "scratches", readOnly = TRUE)
      # hot_table(outtab, highlightCol = TRUE, highlightRow = TRUE)
      outtab <- hot_context_menu(outtab, allowRowEdit = FALSE, allowColEdit = FALSE)
      return(outtab)
    }
  }
  NULL
}

review_table_groom <- function(input, paths_day, metadata) {
  sess <- gsub(".*\\((.*)\\).*", "\\1", input$session_for_review)
  fp <- file.path(paths_day$dirpath, paste0(sess, "_groom.csv"))
  if (metadata$focal_sessions_so_far > 0 & file.exists(fp)) {
    outtab <- read.csv(fp)[-1, ]
    # print(head(outtab))
    outtab <- rhandsontable(outtab, rowHeaders = NULL, height = 500)
    # outtab <- hot_col(outtab, "scratches", readOnly = TRUE)
    # hot_table(outtab, highlightCol = TRUE, highlightRow = TRUE)
    outtab <- hot_context_menu(outtab, allowRowEdit = FALSE, allowColEdit = FALSE)
    return(outtab)
  }
  NULL
}

review_table_aggr <- function(input, paths_day, metadata) {
  sess <- gsub(".*\\((.*)\\).*", "\\1", input$session_for_review)
  fp <- file.path(paths_day$dirpath, paste0(sess, "_aggr.csv"))
  if (metadata$focal_sessions_so_far > 0 & file.exists(fp)) {
    outtab <- read.csv(fp)
    # outtab <- outtab[-nrow(outtab), ]
    # print(head(outtab))
    outtab <- rhandsontable(outtab, rowHeaders = NULL, height = 500)
    # outtab <- hot_col(outtab, "scratches", readOnly = TRUE)
    # hot_table(outtab, highlightCol = TRUE, highlightRow = TRUE)
    outtab <- hot_context_menu(outtab, allowRowEdit = FALSE, allowColEdit = FALSE)
    return(outtab)
  }
  NULL
}





