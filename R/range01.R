#' Rescale Numeric Vector to [0, 1] range
#'
#' This function takes a numeric interval
#' and rescales it to be between 0 and 1.
#'
#' @param x A numeric vector to be rescaled.
#' @return A rescaled numeric vector.
#'
#' @export

range01 <- function(x) {
  if (class(x) != "numeric" & class(x) != "integer") {
    stop("x is neither integer or numeric class.")
  }
  return((x - min(x)) / (max(x) - min(x)))
}
