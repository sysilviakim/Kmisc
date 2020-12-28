#' Find the Recentmost File by File Name
#'
#' This function takes a list of files with their full paths and returns the
#' recentmost file by the name and the date embedded in the file name. If
#' necessary, you can also use the outputs of `file.info` function.
#'
#' @importFrom dplyr "%>%"
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr bind_cols
#' @importFrom dplyr bind_rows
#' @importFrom purrr map
#' @importFrom purrr map2
#' @importFrom dplyr tibble
#' @importFrom stringr str_match_all
#' @importFrom readr parse_date
#' @importFrom rlang quo
#'
#' @param path_list List of files to investigate with their full path
#' @param pattern Pattern of date in file names. Defaults to "_([0-9]+).txt$".
#' @param date_format Format of the date, esp. their order.
#' Defaults to "\%Y\%m\%d".
#' @param filter_cols If there are additional conditions that need to be met
#' (dplyr::filter), specify the columns here. If multiple columns, as list.
#' @param filter_conds If there are additional conditions that need to be met
#' (dplyr::filter), specify the conditions here. If multiple columns, as list.
#' This will be used with the \%in\% operator.
#'
#' @export

file_recent <- function(path_list,
                        pattern = "_([0-9]+).txt$",
                        date_format = "%Y%m%d",
                        filter_cols = NULL,
                        filter_conds = NULL) {
  . <- NULL
  out <- path_list %>%
    map(
      ~ {
        if (length(str_match_all(.x, pattern)[[1]]) > 0) {
          tibble(
            path = .x,
            date = str_match_all(.x, pattern)[[1]][1, 2] %>%
              parse_date(format = date_format)
          ) %>%
            bind_cols(., file.info(.x)) %>%
            mutate(
              max_mtime = (mtime == max(mtime)),
              check = (mtime + hours(1) > Sys.time())
            )
        }
      }
    ) %>%
    bind_rows()

  ## Extra filter conditions: because %in% is NA safe
  ## https://stackoverflow.com/questions/49075824/
  if (!is.null(filter_cols)) {
    fp <- map2(
      filter_cols, filter_conds, function(x, y) quo((!!(as.name(x))) %in% !!y)
    )
    out <- filter(out, !!!fp)
  }

  if (nrow(out) > 0) {
    return(
      out %>%
        filter(date == max(date, na.rm = TRUE)) %>%
        .$path
    )
  } else {
    return(NA)
  }
}
