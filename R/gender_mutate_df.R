#' Inferring Gender from First Names
#'
#' This function uses ropensci/gender to convert first names in a dataframe
#' and obtain additional columns from the ropensci/gender outcome.
#' This function complements ropensci/gender to be directly used with dataframes

#' @import gender
#' @import dplyr
#' @param df Dataframe containing a column of first names
#' @param input_name The name of the column containing first names in given df
#' @param years Which years to use for inferring gender: defaults to 1932-2012.
#' @param method Which method to use for inferring gender: defaults to "ssa".
#' @param countries Which country to use as a baseline: defaults to the U.S.
#' @examples
#' df <- data.frame(NameFirst = c("Madison", "Nimrodel", "Michael"), stringsAsFactors = FALSE)
#' gender_mutate_df(df, "NameFirst", years = c(1920, 2010))
#' @export

gender_mutate_df <- function(df, input_name, years = c(1932, 2012),
                             method = "ssa", countries = "United States") {
  if (!is.data.frame(df)) {
    stop("The input is not a dataframe.")
  }
  if (!(input_name %in% names(df))) {
    stop("The specified column does not exist in the input dataframe.")
  }
  if (length(intersect(
    c(
      "proportion_male", "proportion_female",
      "gender", "year_min", "year_max"
    ),
    names(df)
  )) != 0) {
    stop(paste0(
      "The given column names conflict with the gender package's ",
      "output:\n", "proportion_male, proportion_female, gender, ",
      "year_min, and year_max."
    ))
  }
  if (!(method %in%
    c("ssa", "ipums", "napp", "kantrowitz", "genderize", "demo"))) {
    stop("This is not a valid method for gender::gender.")
  }
  if (!countries %in%
    c(
      "United States", "Canada", "United Kingdom",
      "Denmark", "Iceland", "Norway", "Sweden"
    )) {
    stop("This is not a valid country recognized by gender::gender.")
  }
  output <-
    gender::gender(
      names = df[[input_name]],
      years = years,
      method = method,
      countries = countries
    )
  output <- output %>%
    dplyr::group_by(!!as.name("name")) %>%
    dplyr::slice(1)
  names(output)[names(output) == "name"] <- input_name
  output <- dplyr::left_join(df, output, by = input_name)
  if (sum(is.na(output$gender)) > 0) {
    warning(paste0(
      "Out of ", nrow(output), " observations, ",
      sum(is.na(output$gender)),
      " observations were not classified."
    ))
  } else {
    message("All names successfully classified.")
  }
  return(output)
}
