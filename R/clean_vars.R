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
#' @import stringr
#' @param df Dataframe to be cleaned
#' @param varnames All variables to be cleaned
#' @param varnames_date Date variables
#' @param date_order Order of the date variable, if string format
#' @param varnames_num Numeric variables
#' @param firstname Variable containing first names
#' @param prefix Variable containing self-reported personal prefixes
#' @param gender_original Variable containing original gender entry

#' @export

clean_vars <- function(df = NULL,
                       varnames = NULL,
                       varnames_date = NULL,
                       date_order = "mdy",
                       varnames_num = NULL,
                       firstname = NULL,
                       prefix = NULL,
                       gender_original = NULL) {
  . <- proportion_female <- year_min <- year_max <- NULL
  output <- df
  if (!is.null(setdiff(varnames, varnames_date)) &
    length(setdiff(varnames, varnames_date)) > 0) {
    for (x in setdiff(varnames, varnames_date)) {
      output[[x]] <-
        gsub(
          "[ \t]{2,}", " ",
          gsub(
            "^\\s+|\\s+$", "",
            gsub(
              "[[:punct:]]", " ",
              gsub("[^ -~]", "", tolower(output[[x]]))
            )
          )
        )
    }
  }
  if (!is.null(varnames_num)) {
    for (x in varnames_num) {
      output[[x]] <-
        as.numeric(gsub(
          "[^[:digit:]]", "",
          stringr::str_split_fixed(output[[x]], stringr::fixed(" "), 2)[, 1]
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
      (!is.na(output[[prefix]]) & tolower(output[[prefix]]) == "mr"),
      "male", output$gender
    )
  }
  if (!is.null(prefix)) {
    output$gender <- ifelse(
      (!is.na(output[[gender_original]]) &
        tolower(output[[gender_original]]) == "m"),
      "male", output$gender
    )
  }
  return(output)
}
