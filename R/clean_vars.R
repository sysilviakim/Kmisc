#' Variable Cleaning
#'
#' This function takes a dataframe and performs a variety of cleaning.
#' Characters are stripped of unnecessary whitespaces, non-ASCII, and
#' lowercased. Numbers are stripped of non-numeric and turned to numeric.
#' Date variables are parsed. Finally, gender is inferred if first name
#' variable is provided, overwritten by self-reported prefixes and original
#' gender variable, if it exists. Function title_gender_infer is a variation
#' of this function.

#' @import dplyr
#' @importFrom lubridate parse_date_time
#' @importFrom stringi stri_trans_general
#' @importFrom stringr str_split_fixed
#' @importFrom stringr fixed
#'
#' @param df Dataframe to be cleaned.
#' @param varnames All variables to be cleaned.
#' Defaults to NULL.
#' @param varnames_date Date variables.
#' Defaults to NULL.
#' @param date_order Order of the date variable, if string format.
#' Defaults to "mdy".
#' @param varnames_num Numeric variables.
#' Defaults to NULL.
#' @param firstname Variable containing first names.
#' Defaults to NULL.
#' @param prefix Variable containing self-reported personal prefixes.
#' Defaults to NULL.
#' @param prefix_male Value that indicates male in the prefix variable.
#' Defaults to "mr" (the input will be lowercased before comparison.)
#' @param gender_original Variable containing original gender entry.
#' Defaults to NULL.
#' @param gender_male Value that indicates male in the original gender entry.
#' Defaults to "m" (the input will be lowercased before comparison.)

#' @export

clean_vars <- function(df,
                       varnames = NULL,
                       varnames_date = NULL,
                       date_order = "mdy",
                       varnames_num = NULL,
                       firstname = NULL,
                       prefix = NULL,
                       prefix_male = "mr",
                       gender_original = NULL,
                       gender_male = "m") {
  . <- proportion_female <- year_min <- year_max <- NULL
  output <- df
  if (!is.null(setdiff(varnames, varnames_date)) &
    length(setdiff(varnames, varnames_date)) > 0) {
    for (x in setdiff(varnames, varnames_date)) {
      ## ASCII conversion rather than deletion
      ## So no longer gsub("[^ -~]", "", tolower(output[[x]]))
      output[[x]] <- stri_trans_general(tolower(output[[x]]), "latin-ascii")
      output[[x]] <-
        gsub(
          "[ \t]{2,}", " ",
          gsub("^\\s+|\\s+$", "", gsub("[[:punct:]]", " ", output[[x]]))
        )
    }
  }
  if (!is.null(varnames_num)) {
    for (x in varnames_num) {
      output[[x]] <-
        as.numeric(gsub(
          "[^[:digit:]]", "",
          str_split_fixed(output[[x]], fixed(" "), 2)[, 1]
        ))
    }
  }
  if (!is.null(varnames_date)) {
    for (x in varnames_date) {
      output[[x]] <-
        as.Date(parse_date_time(output[[x]], date_order))
    }
  }
  if (!is.null(firstname)) {
    output <- output %>%
      Kmisc::gender_mutate_df(., input_name = firstname) %>%
      dplyr::select(-proportion_female, -year_min, -year_max)
    output$gender <- ifelse(output$gender == "either", NA, output$gender)
  } else {
    output$gender <- NA
  }
  if (!is.null(prefix)) {
    output$gender <- ifelse(
      (!is.na(output[[prefix]]) & tolower(output[[prefix]]) == prefix_male),
      "male", output$gender
    )
  }
  if (!is.null(prefix)) {
    output$gender <- ifelse(
      (!is.na(output[[gender_original]]) &
        tolower(output[[gender_original]]) == gender_male),
      "male", output$gender
    )
  }
  return(output)
}
