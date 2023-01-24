# initiate and update id table for nn

# all_individuals <- read.csv("id_table.csv", stringsAsFactors = FALSE)
# all_individuals$present <- FALSE
# all_individuals$swelling <- factor(NA, levels = c("", 0, 1, 2, 3))
# all_individuals$comment <- ""
#
# ori <- id_table_initiate(all_individuals, "r1", 3)
# xdata <- list(presence = all_individuals[all_individuals$group == "r1", ])
# # add new id
# xdata$presence <- rbind(xdata$presence, NA)
# xdata$presence$present[nrow(xdata$presence)] <- FALSE
# xdata$presence$group[nrow(xdata$presence)] <- xdata$presence$group[1]
# xdata$presence$is_focal[nrow(xdata$presence)] <- "no"
# xdata$presence$id[nrow(xdata$presence)] <- NA
# xdata$presence$sex[nrow(xdata$presence)] <- "f"
# current_presence <- xdata$presence

id_table_update <- function(ori, current_presence) {
  sel <- which(!current_presence$id %in% ori$id & !is.na(current_presence$id))

  out <- ori
  if (length(sel) >= 1) {
    for (i in sel) {
      out <- rbind(out, NA)
      out$id[nrow(out)] <- current_presence$id[i]
      out$sex[nrow(out)] <- current_presence$sex[i]
      out$group[nrow(out)] <- current_presence$group[i]
      out$is_focal[nrow(out)] <- current_presence$is_focal[i]
      out$present[nrow(out)] <- current_presence$present[i]
      out$comment[nrow(out)] <- current_presence$comment[i]
      out$in_nn_tracker[nrow(out)] <- FALSE
      if (is.na(out$sex[nrow(out)])) out$sex[nrow(out)] <- "o"
    }
  }
  out
}

# initial id list for nn data
id_table_initiate <- function(xdata, group, n_age_sex_classes = 5, include_nn_ids = FALSE) {
  out <- xdata[xdata$group == group, ]
  out <- rbind(out, NA)
  out$id[nrow(out)] <- "new1"
  out$sex[nrow(out)] <- "m"
  out$group[nrow(out)] <- group
  out$is_focal[nrow(out)] <- "no"
  out$present[nrow(out)] <- FALSE
  out$comment[nrow(out)] <- ""

  out <- rbind(out, NA)
  out$id[nrow(out)] <- "new2"
  out$sex[nrow(out)] <- "m"
  out$group[nrow(out)] <- group
  out$is_focal[nrow(out)] <- "no"
  out$present[nrow(out)] <- FALSE
  out$comment[nrow(out)] <- ""

  if (include_nn_ids) {
    for (i in 1:4) {
      for (k in 1:n_age_sex_classes) {
        out <- rbind(out, NA)
        out$group[nrow(out)] <- group
        out$is_focal[nrow(out)] <- "no"
        out$present[nrow(out)] <- NA
        out$comment[nrow(out)] <- ""
        out$sex[nrow(out)] <- "o"
      }
      if (i == 1) out$id[nrow(out):(nrow(out) - n_age_sex_classes + 1)] <- paste0(c("AM"), n_age_sex_classes:1)
      if (i == 2) out$id[nrow(out):(nrow(out) - n_age_sex_classes + 1)] <- paste0(c("AF"), n_age_sex_classes:1)
      if (i == 3) out$id[nrow(out):(nrow(out) - n_age_sex_classes + 1)] <- paste0(c("J"), n_age_sex_classes:1)
      if (i == 4) out$id[nrow(out):(nrow(out) - n_age_sex_classes + 1)] <- paste0(c("I"), n_age_sex_classes:1)
    }
  }

  # add additional tracker for monitoring of current nn scan
  out$in_nn_tracker <- FALSE
  out
}
