#' Paste with NA suppression
#'
#' This enables `paste` to work even when some of the entries are NA values,
#' and the desired output is an empty string when NAs are pasted.
#'
#' This is originally from the following StackOverflow post:
#' https://stackoverflow.com/questions/13673894/suppress-nas-in-paste#15673180
#' This is simply a cosmetically changed version of 42-'s answer.
#'
#' @param ...	List of R objects, to be converted to character vectors.
#' @param sep	A character string to separate the terms.
#'
#' @export

paste_na <- function(..., sep = " ") {
  L <- list(...)
  L <- lapply(
    L,
    function(x) {
      x[is.na(x)] <- ""
      x
    }
  )
  out <- gsub(
    paste0("(^", sep, "|", sep, "$)"), "",
    gsub(
      paste0(sep, sep), sep,
      do.call(paste, c(L, list(sep = sep)))
    )
  )
  is.na(out) <- out == ""
  return(out)
}
