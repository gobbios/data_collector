render_nn_new <- function(nndata, metadata, doit = TRUE, buttons_per_row = NULL) {
  if (!doit) return(NULL)
  if (is.null(buttons_per_row)) buttons_per_row <- metadata$setup_nn_buttons_per_row
  cdata <- nndata[nndata$id != metadata$focal_id, ]
  
  ids <- cdata$id
  sex <- cdata$sex
  selected <- cdata$in_nn_tracker
  cat(selected)
  
  perc <- paste0(round(95/buttons_per_row, 2), "%")
  
  btn_ids <- paste0("id_", ids)
  xclass <- character(length(ids))
  sex[is.na(sex)] <- "o"
  selected[is.na(selected)] <- FALSE
  
  
  # colors
  # color values
  xcols <- c("rgba(255, 0, 0, 0.05)", "rgba(255, 0, 0, 0.6)",
            "rgba(0, 0, 255, 0.05)", "rgba(0, 0, 255, 0.6)", 
            "rgba(10, 10, 10, 0.05)", "rgba(10, 10, 10, 0.6)")[(as.numeric(as.factor(sex)) * 2 - 1) + selected]
  
  
  # outer results list
  res <- list()
  
  for (s in 1:3) {
    x <- which(sex == c("f", "m", "o")[s])
    # color values
    # xcol <- xcols[x]
    # layout
    pmat <- matrix(data = c(x, rep(NA, buttons_per_row - length(x) %% buttons_per_row)), ncol = buttons_per_row, byrow = TRUE)
    if (length(x) %% buttons_per_row == 0) pmat <- matrix(data = x, ncol = buttons_per_row, byrow = TRUE)
    
    # inner results
    inner_res <- list()

    for (i in 1:nrow(pmat)) {
      xlist <- lapply(pmat[i, ], function(x){
        if (!is.na(x)) {
          div(style = paste("width:", perc, 
                            "; background-color:", xcols[x],
                            # "; color:", c("black", "white")[selected + 1][x],
                            "; text-align: center",
                            "; border-radius: 20px", 
                            "; font-weight: bolder", 
                            "; font-size: large",
                            "; margin: 12px",
                            "; padding: 12px"), 
              checkboxInput(inputId = btn_ids[x], label = ids[x]))
        } else {
          div(br())
        }
        
      })
      
      inner_res[[length(inner_res) + 1]] <- tagAppendAttributes(class="shiny-split-layout", div(xlist))
      # inner_res[[length(inner_res) + 1]] <- p()
    }
    res[[length(res) + 1]] <- inner_res
    res[[length(res) + 1]] <- p()
    
  }
  res
}