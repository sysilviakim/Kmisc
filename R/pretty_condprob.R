#' Append Row Number as a Variable to a Dataframe
#'
#' This function takes a dataframe and adds a variable of row numbers.
#' This is the simplest function that does not take into account any group
#' structures.
#'
#' @param df Input dataframe.
#' @param A_var The A variable in "the conditional probability of A given B."
#' @param A_val Value of A variable in interest.
#' @param B_var The B variable in "the conditional probability of A given B."
#' @param B_val Value of B variable in interest.
#' @param output Whether to print a numeric number or a statement.
#' If the argument is "string", the function prints a message containing
#' the conditional probability.
#' Defaults to "string."
#' @param digits The digit of the final output. Defaults to 1.
#' @param percent Whether to display the conditional probability in a
#' percentage. Defaults to TRUE.#'
#'
#' @importFrom dplyr "%>%"
#' @importFrom dplyr filter
#'
#' @export

pretty_condprob <- function(df, A_var, A_val, B_var, B_val,
                            output = "string", digits = 1, percent = TRUE) {
  B <- df %>%
    filter(!!as.name(B_var) == B_val)
  A <- df %>%
    filter((!!as.name(A_var) == A_val) & (!!as.name(B_var) == B_val))
  if (output != "string") {
    return(nrow(A) / nrow(B))
  } else {
    message(
      paste0(
        "Cond. on ", B_var, " == ", B_val, ", Pr(",
        A_var, " == ", A_val, ") is ",
        ifelse(
          percent == TRUE,
          paste0(round(nrow(A) / nrow(B) * 100, digits = digits), "%"),
          round(nrow(A) / nrow(B), digits = digits)
        )
      )
    )
  }
}

