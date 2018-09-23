#' Matrix to Dataframe with Row ID Preservation
#'
#' Similar to tibble::rowid_to_column but also converting matrix to dataframe.
#' It can also take lists of matrix/dataframes.
#'
#' @import dplyr
#' @param x The object(s) with row IDs to be converted to dataframes
#' @param colname The column name that will contain the row names
#' @examples
#' mdat <- matrix(c(1, 2, "A", "B"), nrow = 2, ncol = 2, byrow = TRUE,
#'                dimnames = list(c("row1", "row2"), c("col1", "col2")))
#' rowid_matrix_to_df(mdat, colname = "row")
#' @export

rowid_matrix_to_df <- function(x, colname = "rownames") {
  temp <- function(x, colname = "rownames") {
    rownames <- rownames(x)
    x <- dplyr::as_data_frame(x)
    x[[colname]] <- rownames
    x <- x %>% dplyr::select(!!as.name(colname), dplyr::everything())
    return(x)
  }
  if ("list" %in% class(x)) {
    output <- lapply(x, function(y) temp(y, colname))
    return(output)
  } else {
    return(temp(x, colname))
  }
}
