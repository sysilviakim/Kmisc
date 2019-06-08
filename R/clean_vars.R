#' Variable Cleaning
#'
#' This function takes a dataframe and performs a variety of cleaning.
#' Characters are stripped of unnecessary whitespaces, non-ASCII, and
#' lowercased. Numbers are stripped of non-numeric and turned to numeric.
#' Date variables are parsed. Finally, gender is inferred if first name
#' variable is provided, overwritten by self-reported prefixes and original
#' gender variable, if it exists. Function title_gender_infer is a variation
#' of this function.

#' @importFrom dplyr "%>%"
#' @importFrom dplyr select
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
#' @param gender Whether to create an inferred gender variable.
#' Defaults to TRUE.
#' @param firstname Variable containing first names.
#' Defaults to NULL.
#' @param prefix Variable containing self-reported personal prefixes.
#' Defaults to NULL.
#' @param prefix_male Regex expression that indicates male in the prefix
#' variable. Defaults to "^mr$"
#' (the input will be lowercased before comparison.)
#' @param prefix_female Regex expressions that indicate female in the prefix
#' variable. Defaults to "^ms$|^miss$|^mrs$"
#' (the input will be lowercased before comparison.)
#' @param gender_original Variable containing original gender entry.
#' Defaults to NULL.
#' @param gender_male Regex expression that indicates male
#' in the original gender entry. Defaults to "^m$|^male$"
#' (the input will be lowercased before comparison.)
#' @param gender_female Regex expression that indicates female
#' in the original gender entry. Defaults to "^f$|^female$"
#' (the input will be lowercased before comparison.)

#' @export

clean_vars <- function(df,
                       varnames = NULL,
                       varnames_date = NULL,
                       date_order = "mdy",
                       varnames_num = NULL,
                       firstname = NULL,
                       gender = TRUE,
                       prefix = NULL,
                       prefix_male = "^mr$",
                       prefix_female = "^ms$|^miss$|^mrs$",
                       gender_original = NULL,
                       gender_male = "^m$|^male$",
                       gender_female = "^f$|^female$") {
  . <- proportion_female <- year_min <- year_max <- NULL
  output <- df
  if (nrow(output) > 0) {
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
    if (gender == TRUE) {
      if ("gender" %in% names(output)) {
        message("There already exists a variable called 'gender'.")
      } else {
        if (!is.null(firstname)) {
          output <- output %>%
            gender_mutate_df(., input_name = firstname) %>%
            select(-proportion_female, -year_min, -year_max)
          output$gender <- ifelse(output$gender == "either", NA, output$gender)
        } else {
          output$gender <- NA
        }
        if (!is.null(prefix)) {
          output$gender <- ifelse(
            (!is.na(output[[prefix]]) &
              grepl(prefix_male, tolower(output[[prefix]]))),
            "male", output$gender
          )
          output$gender <- ifelse(
            (!is.na(output[[prefix]]) &
               grepl(prefix_female, tolower(output[[prefix]]))),
            "female", output$gender
          )
        }
        if (!is.null(gender_original)) {
          output$gender <- ifelse(
            (!is.na(output[[gender_original]]) &
              grepl(gender_male, tolower(output[[gender_original]]))),
            "male", output$gender
          )
          output$gender <- ifelse(
            (!is.na(output[[gender_original]]) &
               grepl(gender_female, tolower(output[[gender_original]]))),
            "female", output$gender
          )
        }
      }
    }
  }
  return(output)
}
