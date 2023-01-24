cat_table <- function(df, head = TRUE) {
  if (head) df <- head(df)
  df <- as.matrix(df)
  df <- rbind(colnames(df), df)
  df <- cbind(c("cn", substr(rownames(df)[-1], 1, 4)), df)
  cat(paste(apply(df, 1, paste0, collapse = "\t"), collapse = "\n"), "\n")
}
