#' Load RData into a Pre-Specified Assignment
#'
#' This function is from [StackOverflow post 5577221](https://stackoverflow.com/questions/5577221/how-can-i-load-an-object-into-a-variable-name-that-i-specify-from-an-r-data-file) here,
#' Ricardo's answer.
#' It loads an Rda file and enables it to be assigned to a specific object,
#' instead of using the object name that it was stored with.
#' This is an incredibly useful function that deserves a place in base R,
#' but until then I will use it here.

#' @param file_name The file name of the .RData object.
#' @export

loadRData <- function(file_name){
  load(file_name)
  get(ls()[ls() != "file_name"])
}
