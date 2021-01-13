#' Create Percentage Tables
#'
#' This function takes a dataframe and creates a table in percentages.
#' This is an enhanced version of \code{prop.table}.
#'
#' @param df Input dataframe.
#' @param vars Variables to be put into \code{table}.
#' @param digits Digits under the decimal point to show.
#' Defaults to 1.
#'
#' @export

prop <- function(df, vars, digits = 1) {
  if (length(vars) > 2) {
    stop("Too many variables.")
  }
  if (length(vars) < 1) {
    stop("Invalid vars argument.")
  }
  if (length(vars) == 1) {
    print(
      formatC(
        prop.table(table(df[[vars]], dnn = vars)) * 100,
        format = "f", digits = digits
      ),
      quote = FALSE
    )
  }
  if (length(vars) == 2) {
    print(
      formatC(
        prop.table(
          table(df[[vars[1]]], df[[vars[2]]], dnn = vars)
        ) * 100,
        format = "f", digits = digits
      ),
      quote = FALSE
    )
  }
}
