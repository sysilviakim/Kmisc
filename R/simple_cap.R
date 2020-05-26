#' Simple Capitalization
#'
#' This function takes a string and outputs
#' a string with only the first character capitalized.
#' This was taken from the following link:
#' https://stackoverflow.com/questions/6364783/capitalize-the-first-letter-of-both-words-in-a-two-word-string

#' @param x This is a character or a vector of characters
#' @return A character or a vector of characters with first char capitalized.
#'
#' @examples
#' simple_cap("attila marcel")
#'
#' @export

simple_cap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  output <- paste(
    toupper(substring(s, 1, 1)),
    substring(s, 2),
    sep = "",
    collapse = " "
  )
  return(output)
}
