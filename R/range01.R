#' Rescale Numeric Vector to [0, 1] range
#'
#' This function takes a numeric interval
#' and rescales it to be between 0 and 1.
#'
#' @param x A numeric vector to be rescaled.
#' @param na.rm A logical value indicating whether to remove NA values before computation. Default is FALSE.
#'
#' @return A rescaled numeric vector.
#'
#' @export

range01 <- function(x, na.rm = FALSE) {
  if (is.numeric(x) == FALSE & is.integer(x) == FALSE) {
    stop("x is neither integer or numeric class.")
  }
  return(
    (x - min(x, na.rm = na.rm)) /
      (max(x, na.rm = na.rm) - min(x, na.rm = na.rm))
  )
}
