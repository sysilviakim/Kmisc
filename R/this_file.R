#' Locating R Script File
#'
#' This function locates the running script's location.
#' The code is from steamer25's StackOverflow post
#' here: https://stackoverflow.com/questions/1815606/rscript-determine-path-of-the-executing-script
#' This enables both sourcing and Rscript-ing.
#'
#' @export

this_file <- function() {
  cmdArgs <- commandArgs(trailingOnly = FALSE)
  needle <- "--file="
  match <- grep(needle, cmdArgs)
  if (length(match) > 0) {
    # Rscript
    return(normalizePath(sub(needle, "", cmdArgs[match])))
  } else {
    # 'source'd via R console
    return(normalizePath(sys.frames()[[1]]$ofile))
  }
}
