
generate_focal_progress <- function(metadata) {
  message1 <- paste(metadata$progr_act, "of", metadata$focal_duration, "done")
  message2 <- paste(metadata$consecutive_oos, "trailing oos values")
  
  xcol <- "rgba(10, 10, 10, 0.3)"
  if (isTRUE(metadata$session_limit_reached)) xcol <- "rgba(255, 0, 0, 0.8)"
  xstyle <- paste0("border-radius: 10px; background-color:", xcol, "; text-align: center; color: white; font-weight: bolder; font-size: large")
  
  tags$div(
    tagList(h6("progress"),
            p(message1),
            p(message2),
    ),
    style = xstyle
  )
}

