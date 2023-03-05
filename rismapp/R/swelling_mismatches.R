swelling_mismatches <- function(censusdata) {
  res <- FALSE
  
  censusdata$dateid <- paste(censusdata$id, as.character(censusdata$date))
  aux1 <- table(censusdata$dateid)
  if (any(aux1 > 1)) {
    xnames <- names(aux1)[aux1 > 1]
    cdata <- censusdata[censusdata$dateid %in% xnames, ]
    cdata$observer <- unlist(lapply(strsplit(cdata$collection_id, "_"), function(x)rev(x)[1]))
    cdata <- cdata[order(cdata$dateid), ]
    cdata$problem <- FALSE
    for (i in unique(cdata$dateid)) {
      aux2 <- cdata[cdata$dateid == i, ]
      aux3 <- table(aux2$swelling, useNA = "no")
      if (length(aux3) != 1) cdata$problem[cdata$dateid == i] <- TRUE
      if (all(is.na(aux2$swelling))) cdata$problem[cdata$dateid == i] <- FALSE
    }
    cdata <- cdata[cdata$problem, ]
    if (nrow(cdata) > 1) {
      res <- cdata[, c("date", "id", "swelling", "observer")]
    }  
  }
  
  res
}
