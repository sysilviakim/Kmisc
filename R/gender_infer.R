#' Inferring Gender from First Names
#'
#' This function uses ropensci/gender to convert first names in a dataframe
#' and obtain additional columns from the ropensci/gender outcome.
#' This function complements ropensci/gender to be directly used with dataframes
#' 
#' @importFrom dplyr "%>%"
#' @importFrom dplyr group_by
#' @importFrom dplyr slice
#' @importFrom dplyr left_join
#'
#' @param df Dataframe containing a column of first names
#' @param input_name The name of the column containing first names in given df
#' @param years Which years to use for inferring gender: defaults to 1932-2012.
#' @param method Which method to use for inferring gender: defaults to "ssa".
#' @param countries Which country to use as a baseline: defaults to the U.S.
#'
#' @examples
#' df <- data.frame(
#'   NameFirst = c("Madison", "Nimrodel", "Michael"),
#'   stringsAsFactors = FALSE
#' )
#' gender_mutate_df(df, "NameFirst", years = c(1920, 2010))
#' @export

gender_mutate_df <- function(df,
                             input_name,
                             years = c(1932, 2012),
                             method = "ssa",
                             countries = "United States") {
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
  output <- gender::gender(
    names = df[[input_name]],
    years = years,
    method = method,
    countries = countries
  )
  output <- output %>%
    group_by(!!as.name("name")) %>%
    slice(1)
  names(output)[names(output) == "name"] <- input_name
  output <- left_join(df, output, by = input_name)
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

#' Inferring Gender from Personal Titles
#'
#' This function takes a dataframe with a column of personal titles,
#' such as Ms. Miss. Mrs. Mr.
#' and outputs another column with respectively inferred gender.

#' @importFrom rlang ":="
#' @importFrom dplyr mutate
#' @importFrom dplyr "%>%"
#'
#' @param df Dataframe containing a column of titles
#' @param input_name The name of the column containing titles in the given df
#' @param output_name The name of the column that will contain inferred gender
#'
#' @keywords gender
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
    mutate(
      !!as.name(output_name) := tolower(!!as.name(input_name)),
      !!as.name(output_name) := gsub("[[:punct:]]", "", !!as.name(output_name)),
      !!as.name(output_name) := gsub("[^ -~]", "", !!as.name(output_name)),
      !!as.name(output_name) := gsub("^\\s+|\\s+$", "", !!as.name(output_name)),
      !!as.name(output_name) := gsub("[ \t]", "", !!as.name(output_name)),
      !!as.name(output_name) :=
        ifelse(!!as.name(output_name) %in% c("miss", "ms", "mrs"), "female",
          ifelse(!!as.name(output_name) %in% c("mr"), "male", NA)
        )
    )
  return(output)
}
