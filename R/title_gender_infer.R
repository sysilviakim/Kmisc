#' Inferring Gender from Personal Titles
#'
#' This function takes a dataframe with a column of personal titles,
#' such as Ms. Miss. Mrs. Mr.
#' and outputs another column with respectively inferred gender.

#' @import rlang
#' @import gender
#' @import dplyr
#' @param df Dataframe containing a column of titles
#' @param input_name The name of the column containing titles in the given df
#' @param output_name The name of the column that will contain inferred gender
#' @keywords title
#' @examples
#' title_gender_infer(data.frame(title = c("Mr.", "Mrs")), "title")
#' @export

title_gender_infer <- function(df, input_name, output_name = "title_gender") {
  if (!is.data.frame(df)) {
    stop("The input is not a dataframe.")
  }
  if (!(input_name %in% names(df))) {
    stop("The specified input column does not exist in the input dataframe.")
  }
  if (output_name %in% names(df)) {
    stop("The specified output column already exists in the input dataframe.")
  }
  output <- df %>%
    dplyr::mutate(
      !!as.name(output_name) := tolower(!!as.name(input_name)),
      !!as.name(output_name) := gsub("[[:punct:]]", "", !!as.name(output_name)),
      !!as.name(output_name) := gsub("[^ -~]",      "", !!as.name(output_name)),
      !!as.name(output_name) := gsub("^\\s+|\\s+$", "", !!as.name(output_name)),
      !!as.name(output_name) := gsub("[ \t]",       "", !!as.name(output_name)),
      !!as.name(output_name) :=
        ifelse(!!as.name(output_name) %in% c("miss", "ms", "mrs"), "female",
               ifelse(!!as.name(output_name) %in% c("mr"), "male", NA))
    )
  return(output)
}
