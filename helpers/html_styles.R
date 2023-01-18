# some HTML style functions

html_boxes <- function(stylename = c("bg-f", "bg-fsel", "bg-m", "bg-msel", "bg-o", "bg-osel")) {
  if (stylename == "bg-f") xcol <- "rgba(255, 0, 0, 0.05)"
  if (stylename == "bg-fsel") xcol <- "rgba(255, 0, 0, 0.6)"
  if (stylename == "bg-m") xcol <- "rgba(0, 0, 255, 0.05)"
  if (stylename == "bg-msel") xcol <- "rgba(0, 0, 255, 0.6)"
  if (stylename == "bg-o") xcol <- "rgba(10, 10, 10, 0.05)"
  if (stylename == "bg-osel") xcol <- "rgba(10, 10, 10, 0.6)"

  xcolfont <- ifelse(grepl("sel$", stylename), "white", "black")

  xstring <- paste0(".", stylename, " { text-align: center; border-radius: 50px; background-color: ", xcol, "; padding: 10px; color: ", xcolfont, "; font-weight: bolder; font-size: large; } ", collapse = "")

  # input[type='checkbox']{ width: 30px; height: 30px; line-height: 30px;}

  HTML(xstring)
}


