census_table_to_hot <- function(x) {
  xxx <- x
  # make sure comment column remains character
  xxx$comment[is.na(xxx$comment)] <- ""
  xtab <- rhandsontable(xxx, rowHeaders = NULL)
  # make certain cells/columns read-only
  if ("sex" %in% colnames(xxx)) xtab <- hot_col(xtab, col = "sex", readOnly = TRUE)
  if ("swelling" %in% colnames(xxx)) {
    xtab <- hot_col(xtab, col = "swelling", type = "dropdown", source = as.character(c("", 0:3)))
    swell_col <- which(colnames(x) == "swelling")
    for (i in which(x$sex == "m")) xtab <- hot_cell(xtab, row = i, col = swell_col, readOnly = TRUE)
  }
  # 'hide' columns
  if ("is_focal" %in% colnames(xxx)) xtab <- hot_col(xtab, col = c("is_focal"), colWidths = 0.1)
  if ("in_nn_tracker" %in% colnames(xxx)) xtab <- hot_col(xtab, col = c("in_nn_tracker"), colWidths = 0.1)
  
  hot_table(xtab, highlightCol = TRUE, highlightRow = TRUE)
}
