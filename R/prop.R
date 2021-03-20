#' Create Percentage Tables
#'
#' This function takes a dataframe and creates a table in percentages.
#' This is an enhanced version of \code{prop.table}.
#'
#' @param df Input dataframe.
#' @param vars Variables to be put into \code{table}.
#' @param digit Digits under the decimal point to show. Defaults to 1.
#' @param sort Whether to sort the table. Defaults to NULL.
#' @param head Whether to restrict the table to the top few observations.
#' Defaults to NULL.
#' @param print Whether to only print the output, so that no quotes are seen
#' after applying \code{formatC}.
#' @param useNA useNA which is an argument for \code{table}.
#' Defaults to "ifany."
#'
#' @export

prop <- function(df, vars, digit = 1, sort = NULL, head = NULL, print = TRUE,
                 useNA = "ifany") {
  if (length(vars) > 2) {
    stop("Too many variables.")
  }
  if (length(vars) < 1) {
    stop("Invalid vars argument.")
  }
  if (!(useNA %in% c("no", "ifany", "always"))) {
    stop("Invalid useNA argument.")
  }

  if (length(vars) == 1) {
    temp <- prop.table(table(df[[vars]], dnn = vars, useNA = useNA)) * 100
  }
  if (length(vars) == 2) {
    temp <- prop.table(
      table(df[[vars[1]]], df[[vars[2]]], dnn = vars, useNA = useNA)
    ) * 100
  }

  if (!is.null(sort)) {
    temp <- sort(temp, decreasing = sort)
  }
  if (!is.null(head)) {
    temp <- head(temp, head)
  }

  temp <- formatC(temp, format = "f", digits = digit)
  if (print) {
    print(temp, quote = FALSE)
  } else {
    return(temp)
  }
}
