focal_grooming_start_dialog <- function(focal_id, partners) {
  modalDialog("enter grooming information",
              selectInput('grooming_focal_name', "focal subject", choices = c(focal_id, partners), selected = focal_id),
              radioButtons("grooming_focal_approach", "focal approached?", choices = c("unk", "yes", "no"), selected = "unk"),
              radioButtons("grooming_focal_init", "focal iniitated?", choices = c("unk", "yes", "no"), selected = "unk"),
              radioButtons("grooming_focal_direction", "focal gives or receives?", choices = c("gives", "receives", "mutual"), selected = NULL),
              selectInput('grooming_partner_name', "partner", choices = partners[partners != focal_id], selected = NULL),
              
              footer = tagList(
                modalButton("Cancel"),
                actionButton("start_grooming", "start")
              )
  )
}

