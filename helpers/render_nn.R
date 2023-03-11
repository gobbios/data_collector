

ammend_nn_from_census <- function(nn, census) {
  # merge any new information from census (added during the day) to nn
  # to be executed whenever census is modified

  # for now: only possible to edit the last row

  if (is.null(nn)) return(nn)

  # ids that are in census:
  ids <- census$id[!is.na(census$id)]
  # but are not yet in nn
  ids <- ids[!ids %in% nn$id]
  # filter out individuals from added group(s), if any
  # ...
  # if (length(ids) > 0) {
  #   if (length(table(census$group)) > 1) {
  #     ori_group <- census$group[1]
  #     census$group[is.na(census$group)] <- ori_group
  #     ids <- ids[which(census$group == ori_group)]
  #   }
  # }

  # add ids to nn
  if (length(ids) > 0) {
    if (length(ids) > 1) stop("more than one id added in census?")
    nn <- rbind(nn, NA)
    nn$id[nrow(nn)] <- ids
    nn$is_focal[nrow(nn)] <- "no"
    nn$in_nn_tracker[nrow(nn)] <- FALSE
    nn$comment[nrow(nn)] <- ""
    # nn$sex[nrow(nn)] <- census$sex[census$id == nn$id[nrow(nn)]]
  }
  # go through all census ids and update sex in nn accordingly
  for (i in census$id) {
    if (i %in% nn$id) {
      xline <- which(census$id == i)
      if (is.na(census$sex[xline])) {
        nn$sex[nn$id == i] <- "o"
      } else {
        nn$sex[nn$id == i] <- census$sex[xline]
      }
    }
  }

  nn
}

render_nn <- function(ids, selected, sex, do_which = c("f", "m", "o"), buttons_per_row = 6) {
  buttons_per_row <- as.numeric(buttons_per_row)
  btn_ids <- paste0("id_", ids)
  xclass <- character(length(ids))
  sex[is.na(sex)] <- "o"
  selected[is.na(selected)] <- FALSE
  # print(selected)
  # print(sex)
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
  pmat <- matrix(data = c(x, rep(NA, buttons_per_row - length(x) %% buttons_per_row)), ncol = buttons_per_row, byrow = TRUE)
  if (length(x) %% buttons_per_row == 0) pmat <- matrix(data = x, ncol = buttons_per_row, byrow = TRUE)
  
  if (buttons_per_row == 6) xncol <- 2
  if (buttons_per_row == 4) xncol <- 3
  if (buttons_per_row == 12) xncol <- 1
  
  o <- apply(pmat, 2, function(Y) {
    column(xncol, lapply(na.omit(Y), function(X) tagAppendAttributes(checkboxInput(btn_ids[X], ids[X], value = selected[X]), class = xclass[X])))
  })
  o
  
  # that doesn't quite work... the UI is fine but its responsiveness is bizarre...
  # o <- apply(pmat, 1, function(x){
  #   lapply(na.omit(x), function(X) tagAppendAttributes(checkboxInput(btn_ids[X], ids[X]), class = xclass[X], style = "width: 6%"))
  # })
  # 
  # lapply(o, function(x) tagAppendAttributes(class="shiny-split-layout", div(x)))
  # 
}
