#' Mode Function
#'
#' This is a function that outputs a mode of a sgiven vector
#' This is named Mode instead of mode to prevent conflict with base::mode
#' This was taken from Stackoverflow:
#' https://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode
#'
#' @param x The input vector to compute the mode
#' @param na.rm Whether to remove NA values. Defaults to TRUE.
#' @keywords mode
#' @examples
#' x <- c(1, 2, 2, 2, 3, 3, NA)
#' Mode(x)
#' @export

Mode <- function(x, na.rm = T) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

