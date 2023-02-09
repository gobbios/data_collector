review_table_adlib_agg <- function(metadata) {
  fp <- file.path(metadata$adlib_aggr)
  if (file.exists(fp)) {
    outtab <- read.csv(fp)
    # print(fp)
    outtab <- rhandsontable(outtab, rowHeaders = NULL, height = 500, readOnly = TRUE)
    outtab <- hot_context_menu(outtab, allowRowEdit = FALSE, allowColEdit = FALSE)
    return(outtab)
  }
  NULL
}

review_table_foctab <- function(input, metadata) {
  sess <- gsub(".*\\((.*)\\).*", "\\1", input$session_for_review)
  fp <- file.path(metadata$day_dir, paste0(sess, "_foctab.csv"))
  if (file.exists(fp)) {
    # outtab <- read.csv(paste0("www/", sess, ".csv"))
    outtab <- read.csv(fp)
    # print(fp)
    outtab <- rhandsontable(outtab, rowHeaders = NULL, height = 500, readOnly = TRUE)
    # outtab <- hot_col(outtab, "scratches", readOnly = TRUE)
    # hot_table(outtab, highlightCol = TRUE, highlightRow = TRUE)
    outtab <- hot_context_menu(outtab, allowRowEdit = FALSE, allowColEdit = FALSE)
    return(outtab)
  }
  NULL
}


review_table_nn <- function(input, metadata) {
  sess <- gsub(".*\\((.*)\\).*", "\\1", input$session_for_review)
  fp <- file.path(metadata$day_dir, paste0(sess, "_nn.csv"))
  if (file.exists(fp)) {
    if (!isTRUE(readLines(fp) == "")) {
      outtab <- read.csv(fp)
      # colnames(outtab) <- c("id", paste0("scan", seq_len(ncol(outtab) - 1)))
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

review_table_groom <- function(input, metadata) {
  sess <- gsub(".*\\((.*)\\).*", "\\1", input$session_for_review)
  fp <- file.path(metadata$day_dir, paste0(sess, "_groom.csv"))
  if (file.exists(fp)) {
    outtab <- read.csv(fp)
    # print(head(outtab))
    outtab <- rhandsontable(outtab, rowHeaders = NULL, height = 500)
    # outtab <- hot_col(outtab, "scratches", readOnly = TRUE)
    # hot_table(outtab, highlightCol = TRUE, highlightRow = TRUE)
    outtab <- hot_context_menu(outtab, allowRowEdit = FALSE, allowColEdit = FALSE)
    return(outtab)
  }
  NULL
}

review_table_aggr <- function(input, metadata) {
  sess <- gsub(".*\\((.*)\\).*", "\\1", input$session_for_review)
  fp <- file.path(metadata$day_dir, paste0(sess, "_aggr.csv"))
  if (file.exists(fp)) {
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





