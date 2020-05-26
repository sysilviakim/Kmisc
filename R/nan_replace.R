#' Check NaN values in a dataframe and replace them
#'
#' This function takes a dataframe and checks for NaN values.
#' It also replaces the NaN values if requested, a with user-specified value.
#' This function was partially taken from the following StackOverflow link:
#' https://stackoverflow.com/questions/18142117/how-to-replace-nan-value-with-zero-in-a-huge-data-frame/18143097
#'
#' @param df Dataframe to be checked for NaN values
#' @param replace Whether to replace NaN values.
#' @param value Value to replace NaN with.
#'
#' @examples
#' df <- data.frame(a = c(1, 2), b = c(NaN, 3))
#' nan_replace(df)
#'
#' @export

nan_replace <- function(df, replace = TRUE, value = 0) {
  is.nan.data.frame <- function(x) {
    do.call(cbind, lapply(x, is.nan))
  }
  if (replace == TRUE) {
    df[is.nan(df)] <- 0
    return(df)
  } else {
    return(is.nan(df))
  }
}
