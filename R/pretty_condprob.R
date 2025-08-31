#' Conditional probability Pr(A = A_val | B = B_val)
#'
#' Compute the conditional probability of event A = A_val given B = B_val
#' from a data frame. Missing values (NA) can be treated as valid categories
#' or dropped according to the `na_policy` option.
#'
#' @param df A data.frame or tibble containing variables \code{A_var} and \code{B_var}.
#' @param A_var String. Name of the A variable.
#' @param A_val Scalar (may be \code{NA}). Target value of A for the numerator.
#' @param B_var String. Name of the B variable.
#' @param B_val Scalar (may be \code{NA}). Conditioning value of B (defines the denominator set).
#' @param na_policy Character; one of \code{"keep_all"}, \code{"drop_A"}, \code{"drop_B"},
#'   \code{"drop_any"}, or \code{"drop_if_both"}. Short synonyms are accepted (case-insensitive):
#'   \itemize{
#'     \item \code{"keep_all"}: keep all rows. Synonyms: \code{"keep"}, \code{"none"}, \code{"all"}.
#'     \item \code{"drop_A"}: drop rows with A = NA. Synonyms: \code{"A"}.
#'     \item \code{"drop_B"}: drop rows with B = NA. Synonyms: \code{"B"}.
#'     \item \code{"drop_any"}: drop rows where either A or B is NA (complete cases).
#'           Synonyms: \code{"any"}, \code{"complete"}, \code{"complete_cases"}, \code{"cc"}.
#'     \item \code{"drop_if_both"}: drop rows only when both A and B are NA simultaneously.
#'           Synonyms: \code{"pair"}, \code{"if_both"}, \code{"both"}.
#'   }
#' @param output \code{"string"} to print a message; any other value returns the numeric probability.
#' @param digits Integer. Rounding for display. Default \code{1}.
#' @param percent Logical. If \code{TRUE}, display as a percentage; else as a proportion. Default \code{TRUE}.
#'
#' @return If \code{output != "string"}, a numeric probability. If \code{output == "string"},
#'   prints a message and (invisibly) returns the numeric probability.
#'
#' @examples
#' df <- data.frame(A = c(1, 0, 1, NA, 0, NA), B = c("x", "x", "y", "y", NA, NA))
#'
#' pretty_condprob(df, "A", 1, "B", "x", na_policy = "keep_all")
#' pretty_condprob(df, "A", NA, "B", "x", na_policy = "keep_all")
#'
#' # Default for treating NAs: keep all rows, NA is a valid outcome/condition
#' pretty_condprob(df, "A", 1, "B", NA, na_policy = "keep_all")
#' pretty_condprob(df, "A", 1, "B", NA, na_policy = "drop_A")
#' pretty_condprob(df, "A", 1, "B", NA, na_policy = "drop_any")
#' pretty_condprob(df, "A", 1, "B", NA, na_policy = "drop_if_both")
#'
#' @importFrom rlang sym
#' @importFrom dplyr "%>%"
#' @importFrom dplyr filter
#'
#' @export

pretty_condprob <- function(df, A_var, A_val, B_var, B_val,
                            na_policy = "keep_all",
                            output = "string", digits = 1, percent = TRUE) {
  A_sym <- rlang::sym(A_var)
  B_sym <- rlang::sym(B_var)

  # Normalize na_policy (accept short synonyms)
  normalize_na_policy <- function(x) {
    x <- tolower(as.character(x))
    if (x %in% c("keep_all", "keep", "none", "all")) return("keep_all")
    if (x %in% c("drop_a", "a"))                     return("drop_A")
    if (x %in% c("drop_b", "b"))                     return("drop_B")
    if (x %in% c("drop_any", "any", "complete", "complete_cases", "cc"))
      return("drop_any")
    if (x %in% c("drop_if_both", "pair", "if_both", "both"))
      return("drop_if_both")
    stop('Invalid na_policy. Use one of: "keep_all","drop_A","drop_B","drop_any","drop_if_both". ',
         'Short synonyms allowed: "keep","none","all","A","B","any","complete","cc","pair","both","if_both".',
         call. = FALSE)
  }
  na_policy <- normalize_na_policy(na_policy)

  # Apply NA filtering per policy
  if (na_policy == "drop_A") {
    df <- df %>% dplyr::filter(!is.na(!!A_sym))
  } else if (na_policy == "drop_B") {
    df <- df %>% dplyr::filter(!is.na(!!B_sym))
  } else if (na_policy == "drop_any") {
    df <- df %>% dplyr::filter(!is.na(!!A_sym) & !is.na(!!B_sym))
  } else if (na_policy == "drop_if_both") {
    df <- df %>% dplyr::filter(!(is.na(!!A_sym) & is.na(!!B_sym)))
  }

  # Equality that treats NA as a valid value
  match_val <- function(x, val) if (is.na(val)) is.na(x) else x == val

  # Conditioning and joint sets
  B_set   <- df %>% dplyr::filter(match_val(!!B_sym, B_val))
  A_and_B <- df %>% dplyr::filter(match_val(!!A_sym, A_val) & match_val(!!B_sym, B_val))

  # Empty denominator guard
  if (nrow(B_set) == 0) {
    warning("No observations in the conditioning set (B). Returning NA.")
    return(NA_real_)
  }

  prob <- nrow(A_and_B) / nrow(B_set)

  # Output
  fmt <- function(v) if (is.na(v)) "NA" else as.character(v)
  if (output != "string") {
    return(prob)
  } else {
    msg <- paste0(
      "Cond. on ", B_var, " == ", fmt(B_val), ", Pr(",
      A_var, " == ", fmt(A_val), ") is ",
      if (percent) paste0(round(prob * 100, digits = digits), "%")
      else round(prob, digits = digits)
    )
    message(msg)
    return(invisible(prob))
  }
}
