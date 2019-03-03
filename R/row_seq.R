#' Append Row Number as a Variable to a Dataframe
#'
#' This function takes a dataframe and adds a variable of row numbers.
#' This is the simplest function that does not take into account any group
#' structures.
#'
#' @param df Input dataframe.
#' @param row Name for the row variable.
#'
#' @export

row_seq <- function(df, row = "row") {
  if (grepl(row, names(df))) {
    stop("Set another name for the row variable to be created.")
  }
  if (nrow(df) > 0) {
    df[row] <- seq(nrow(df))
  }
  return(df)
}
