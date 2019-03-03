#' Return a Deduplicated a Dataframe
#'
#' This function takes a dataframe and returns only the deduplicated rows,
#' based on all columns.
#'
#' @param df Dataframe to be deduplicated.
#' @param vars Variables to use in deduplication.
#'
#' @export

dedup <- function(df, vars = NULL) {
  if (is.null(vars)) vars <- names(df)
  return(df[!duplicated(df[, vars]), ])
}
