render_nn <- function(ids, selected, sex, do_which = c("f", "m", "o")) {
  btn_ids <- paste0("id_", ids)
  xclass <- character(length(ids))
  xclass[sex == "f"] <- c("bg-f", "bg-fsel")[selected[sex == "f"] + 1]
  xclass[sex == "m"] <- c("bg-m", "bg-msel")[selected[sex == "m"] + 1]
  xclass[sex == "o"] <- c("bg-o", "bg-osel")[selected[sex == "o"] + 1]
  
  x <- which(sex == "f" | sex == "m" | sex == "o")
  if (do_which == "f") {
    x <- which(sex == "f")
  } else {
    if (do_which == "m") {
      x <- which(sex == "m")
    } else {
      x <- which(sex == "o")
    }
  }
  pmat <- matrix(data = c(x, rep(NA, 6 - length(x) %% 6)), ncol = 6, byrow = TRUE)
  if (length(x) %% 6 == 0) pmat <- matrix(data = x, ncol = 6, byrow = TRUE)
  
  o <- apply(pmat, 2, function(Y) {
    column(2, lapply(na.omit(Y), function(X) tagAppendAttributes(checkboxInput(btn_ids[X], ids[X], value = selected[X]), class = xclass[X])))
  })
  o
}
