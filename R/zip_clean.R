#' Clean Zip Codes
#'
#' This function takes a dataframe or vector containing zip codes and convert
#' them to the standard five-digit zip codes. Depending on the argument, it
#' saves the subdigits.
#'
#' @importFrom stringr str_sub
#' @importFrom stringr str_pad
#' @importFrom purrr map
#' @importFrom purrr map2
#'
#' @param df Input dataframe or vector.
#' @param zip_var Variable that contains zip codes, if df is dataframe.
#' @param keep_last_four Whether to keep the last four digits.
#' @param last_four_label Variable to store last four digits of zip, if any.
#'
#' @examples
#' zip_clean(
#'   c("1000-2000", NA, "91125", "911250401", "800", "A", "100A"),
#'   zip_var = "contributor_zip", keep_last_four = TRUE,
#'   last_four_label = "contributor_zip_last4"
#' )
#'
#' @export

zip_clean <- function(df,
                      zip_var = NULL,
                      keep_last_four = FALSE,
                      last_four_label = NULL) {

  if (keep_last_four == TRUE & is.null(last_four_label)) {
    stop("Specify a variable to store the last 4 digits of 9-digit zip codes.")
  }

  if (any(class(df) == "data.frame") | any(class(df) == "tibble")) {
    if (ncol(df) == 0) {
      stop("No columns in dataframe.")
    }
    if (is.null(zip_var)) {
      warning("No zip code variable specified. Using first variable.")
      zip_var <- names(df)[1]
    }
    zip <- as.character(df[[zip_var]])
  } else {
    zip <- df
  }

  ## If dash exists, use that info. Otherwise, rely on nchar
  zip <- gsub("[^0-9-]", "", gsub("-{2,}", "-", zip))
  zip_four <- strsplit(zip, split = "-") %>%
    map(2, .null = NA_integer_) %>%
    unlist()
  zip <- strsplit(zip, split = "-") %>%
    map(1, .null = NA_integer_) %>%
    unlist()

  zip[nchar(zip) < 5 & !is.na(zip)] <- paste0(
    str_pad(zip[nchar(zip) < 5 & !is.na(zip)], width = 5, pad = "0"), "0000"
  )
  zip <- str_pad(zip, width = 9, pad = "0", side = "right")
  zip_four <- map2(
    zip, zip_four, function(.x, .y) ifelse(
      !is.na(.y), .y,
      paste0(str_sub(.x, -4, -1), ifelse(is.na(.y), "", .y))
    )
  ) %>%
    unlist()
  zip <- str_sub(zip, 1, 5)

  zip[zip == "" & !is.na(zip)] <- NA
  zip_four[zip_four == "" & !is.na(zip_four)] <-
    zip_four[zip_four == "0000" & !is.na(zip_four)] <- NA

  if (keep_last_four == TRUE) {
    if (any(class(df) == "data.frame") | any(class(df) == "tibble")) {
      df[[zip_var]] <- zip
      df[[last_four_label]] <- zip_four
      return(df)
    } else {
      temp <- data.frame(zip = zip, zip_four = zip_four)
      return(temp)
    }
  } else {
    if (any(class(df) == "data.frame") | any(class(df) == "tibble")) {
      df[[zip_var]] <- zip
      return(df)
    } else {
      return(zip)
    }
  }
}
