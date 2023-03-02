# generate the UI for grooming
# intended behaviour:
# if no focal active: don't display it at all (seems to be working)
# if focal becomes active: display starting state, i.e. only the start button
# if grooming start button: keep the starting state until start_grooming_new_abtn_1 or _2 button is pressed
# if grooming started: change the appearance
# if grooming ends: bring back starting state

ui_grooming <- function(focal, partner, direction, is_active, grooming1or2) {
  if (grooming1or2 == 1) start_button <- actionButton(paste0("groom_start_", grooming1or2, "_abtn"), paste0("start grooming"))
  if (grooming1or2 == 2) start_button <- actionButton(paste0("groom_start_", grooming1or2, "_abtn"), paste0("add triadic"))
  
  change_give <- actionButton(paste0("groom_give_", grooming1or2, "_abtn"), paste0("focal gives"))
  change_receive <- actionButton(paste0("groom_receive_", grooming1or2, "_abtn"), paste0("focal receives"))
  change_mutual <- actionButton(paste0("groom_mutual_", grooming1or2, "_abtn"), paste0("turns mutual"))
  end_button <- actionButton(paste0("groom_end_", grooming1or2, "_abtn"), paste0("grooming end"))
  
  if (is.na(focal)) return(NULL)
  if (!is_active) {
    res <- tags$div(
      tagList(p(), start_button, p()),
      style = paste0("border-radius: 20px; background-color: rgba(10, 10, 10, 0.4); text-align: center; color: white; font-weight: bolder; font-size: large")
    )
    return(res)
  }
  
  if (direction == 0) xicon <- icon("arrows-left-right")
  if (direction == 1) xicon <- icon("arrow-right")
  if (direction == 2) xicon <- icon("arrow-left")
  if (grooming1or2 == 1) xcol <- "rgba(0, 0, 255, 0.4)"
  if (grooming1or2 == 2) xcol <- "rgba(0, 255, 0, 0.4)"
  
  if (direction == 0) {
    res <- tags$div(
      tagList(
        h3(tagList(focal, xicon, partner)),
        p(),
        change_give,
        change_receive,
        end_button,
        p()
      ),
      style = paste0("border-radius: 20px; background-color: ", xcol, "; text-align: center; color: white; font-weight: bolder; font-size: large")
    )
  }
  if (direction == 1) {
    res <- tags$div(
      tagList(
        h3(tagList(focal, xicon, partner)),
        p(),
        change_receive,
        change_mutual,
        end_button,
        p()
      ),
      style = paste0("border-radius: 20px; background-color: ", xcol, "; text-align: center; color: white; font-weight: bolder; font-size: large")
    )
  }
  if (direction == 2) {
    res <- tags$div(
      tagList(
        h3(tagList(focal, xicon, partner)),
        p(),
        change_give,
        change_mutual,
        end_button,
        p()
      ),
      style = paste0("border-radius: 20px; background-color: ", xcol, "; text-align: center; color: white; font-weight: bolder; font-size: large")
    )
  }
  res
  
}

# updating grooming table and grooming metadata
# and writing grooming data and metadata to file!
groom_monitor <- function(xmeta, grooming1or2, new_direction, grooming_data) {
  rightnow <- as.character(Sys.time())
  newrow <- nrow(grooming_data) + 1
  grooming_data[newrow, ] <- NA
  grooming_data$event_type[newrow] <- "change"
  grooming_data$focal[newrow] <- xmeta$focal_id
  grooming_data$session[newrow] <- xmeta$current_foc_session_id
  if (grooming1or2 == 1) {
    grooming_data$triadic[newrow] <- FALSE
  }
  if (grooming1or2 == 2) {
    grooming_data$triadic[newrow] <- TRUE
  }
  
  if (grooming1or2 == 1) {
    xmeta$groom1_in_progress <- TRUE
    xmeta$groom1_direction <- new_direction
    grooming_data$approach_by_focal[newrow] <- xmeta$groom1_approach_by_focal
    grooming_data$initated_by_focal[newrow] <- xmeta$groom1_initated_by_focal
    
    if (new_direction == 0 | new_direction == 1 | new_direction == 2 | new_direction == -1) xmeta$groom1_event_num <- xmeta$groom1_event_num + 1
    grooming_data$withinevent_num[newrow] <- xmeta$groom1_event_num
    grooming_data$partner[newrow] <- xmeta$groom1_partner
    grooming_data$withinsession_num[newrow] <- xmeta$groom1_session_num
    grooming_data$time_stamp[newrow] <- as.character(xmeta$groom1_time_stamp)
    if (grooming_data$withinevent_num[newrow] > 1 & new_direction != -1) grooming_data$time_stamp[newrow] <- as.character(rightnow)
    
    
    # set direction
    if (isTRUE(xmeta$groom1_direction == 0)) grooming_data$direction[newrow] <- "mutual"
    if (isTRUE(xmeta$groom1_direction == 1)) grooming_data$direction[newrow] <- "gives"
    if (isTRUE(xmeta$groom1_direction == 2)) grooming_data$direction[newrow] <- "receives"
    
    # set event type
    if (isTRUE(xmeta$groom1_event_num == 1)) grooming_data$event_type[newrow] <- "start"
    
    # grooming session end
    if (new_direction == -1) {
      xmeta$groom1_in_progress <- FALSE
      xmeta$groom1_direction <- NA
      xmeta$groom1_time_stamp <- NA
      xmeta$groom1_partner <- NA
      xmeta$groom1_event_num <- 0
      xmeta$groom1_approach_by_focal <- NA
      xmeta$groom1_initated_by_focal <- NA
      grooming_data$leave_by[newrow] <- xmeta$groom1_leave_by_focal
      xmeta$groom1_leave_by_focal <- NA
      grooming_data$event_type[newrow] <- "end"
    }

  }
  
  if (grooming1or2 == 2) {
    xmeta$groom2_in_progress <- TRUE
    xmeta$groom2_direction <- new_direction
    grooming_data$approach_by_focal[newrow] <- xmeta$groom2_approach_by_focal
    grooming_data$initated_by_focal[newrow] <- xmeta$groom2_initated_by_focal
    
    if (new_direction == 0 | new_direction == 1 | new_direction == 2 | new_direction == -1) xmeta$groom2_event_num <- xmeta$groom2_event_num + 1
    grooming_data$withinevent_num[newrow] <- xmeta$groom2_event_num
    grooming_data$partner[newrow] <- xmeta$groom2_partner
    grooming_data$withinsession_num[newrow] <- xmeta$groom2_session_num
    grooming_data$time_stamp[newrow] <- as.character(xmeta$groom2_time_stamp)
    if (grooming_data$withinevent_num[newrow] > 1 & new_direction != -1) grooming_data$time_stamp[newrow] <- as.character(rightnow)
    
    
    # set direction
    if (isTRUE(xmeta$groom2_direction == 0)) grooming_data$direction[newrow] <- "mutual"
    if (isTRUE(xmeta$groom2_direction == 1)) grooming_data$direction[newrow] <- "gives"
    if (isTRUE(xmeta$groom2_direction == 2)) grooming_data$direction[newrow] <- "receives"
    
    # set event type
    if (isTRUE(xmeta$groom2_event_num == 1)) grooming_data$event_type[newrow] <- "start"
    
    # grooming session end
    if (new_direction == -1) {
      xmeta$groom2_in_progress <- FALSE
      xmeta$groom2_direction <- NA
      xmeta$groom2_time_stamp <- NA
      xmeta$groom2_partner <- NA
      xmeta$groom2_event_num <- 0
      xmeta$groom2_approach_by_focal <- NA
      xmeta$groom2_initated_by_focal <- NA
      grooming_data$leave_by[newrow] <- xmeta$groom2_leave_by_focal
      xmeta$groom2_leave_by_focal <- NA
      grooming_data$event_type[newrow] <- "end"
    }
    
  }
  

  if (grooming1or2 == 1) metadata <<- xmeta
  if (grooming1or2 == 2) metadata <<- xmeta
  # write files
  write.csv(grooming_data, file = metadata$active_foc_groom, row.names = FALSE, quote = FALSE)
  write.csv(data.frame(val = unlist(reactiveValuesToList(xmeta))), file = xmeta$day_meta, row.names = TRUE, quote = FALSE)
  # 
  
  
  # metagrooming <<- xmetagrooming
  grooming_data
}

focal_grooming_start_dialog_new <- function(focal_id, partners, grooming1or2) {
  modalDialog(span(paste("time stamp:", as.character(strftime(Sys.time(), format = "%T")))),
              "enter grooming information",
              selectInput('grooming_focal_name', "focal subject", choices = c(focal_id, partners), selected = focal_id),
              radioButtons(paste0("grooming_focal_approach_radio"), "focal approached?", choices = c("unk", "yes", "no"), selected = "unk"),
              radioButtons(paste0("grooming_focal_init_radio"), "focal initiated?", choices = c("unk", "yes", "no"), selected = "unk"),
              radioButtons("grooming_focal_direction", "focal gives or receives?", choices = list("gives" = 1, "receives" = 2, "mutual" = 0), selected = 0),
              selectInput('grooming_partner_name', "partner", choices = partners[partners != focal_id], selected = NULL),
              footer = tagList(
                modalButton("Cancel"),
                actionButton(paste0("start_grooming_new_abtn_", grooming1or2), "start")
              )
  )
}

focal_grooming_end_dialog <- function(grooming1or2) {
  btnid <- paste0("stop_grooming_", grooming1or2, "_abtn")
  
  modalDialog(span(paste("time stamp:", as.character(strftime(Sys.time(), format = "%T")))),
              "enter grooming information",
              radioButtons("grooming_focal_leave", "who leaves?", choices = c("focal", "partner", "unk"), selected = "unk"),
              footer = tagList(
                modalButton("Cancel"),
                actionButton(btnid, "grooming ended")
              )
  )
}


empty_grooming_new <- function() {
  data.frame(time_stamp = character(), # as.POSIXlt(strptime("2000-02-29", format = "%F")),
             session = character(),
             triadic = logical(),
             event_type = character(),
             withinsession_num = integer(),
             withinevent_num = integer(),
             focal = character(),
             partner = character(),
             direction = character(),
             approach_by_focal = logical(),
             initated_by_focal = logical(),
             leave_by = character())
}

