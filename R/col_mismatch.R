#' Return Rows With Different Values Between Columns
#'
#' This function takes a dataframe and if two columns are specified,
#' returns rows with different values between these columns, including
#' instances where one is an NA value.
#'
#' If no columns are specified, it checks for instances of `name_repair` given
#' by the three-dots (https://principles.tidyverse.org/names-attribute.html;
#' see Section 3.5.4 "Ugly, with a purpose").

#' @importFrom dplyr "%>%"
#' @importFrom dplyr tibble
#' @importFrom purrr imap
#' @importFrom purrr map
#' @importFrom dplyr select
#' @importFrom dplyr rowwise
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr group_split
#' @importFrom dplyr across
#' @importFrom rlang "!!"
#' @importFrom rlang set_names
#'
#' @param df Dataframe to be cleaned.
#' @param cols cols
#' Defaults to NULL.
#' @param ref ref

#' @export

col_mismatch <- function(df, cols = NULL, ref = NULL) {
  . <- .y <- root <- NULL

  if (is.null(cols) & all(!grepl("...", names(df)))) {
    message("No columns to compare or merge.")
    return(df)
  } else if (is.null(cols) & any(grepl("...", names(df)))) {
    # Split by "root": for example, a...1 and a...2 both root "a"
    redundant_cols <- tibble(
      raw = names(df)[grepl("...", names(df))],
      root = gsub("\\.\\.\\.[0-9]+$", "", raw)
    ) %>%
      group_split(root) %>%
      `names<-`({
        .
      } %>%
        map(~ .x$root[1]) %>% unlist())

    out <- vector("list", length(redundant_cols)) %>%
      set_names(., names(redundant_cols))

    out <- redundant_cols %>%
      imap(
        ~ {
          temp <- df %>%
            select(.x$raw) %>%
            .[!duplicated(as.list(.))]

          # If only one column, can be merged into single column (all redun.)
          if (ncol(temp) > 1) {
            combinat_cols <- t(combn(names(temp), 2))

            # For each combination
            out[[.y]] <- seq(nrow(combinat_cols)) %>%
              map(
                function(x) mismatch_2cols(
                  df, ref, combinat_cols[x, 1], combinat_cols[x, 2]
                )
              )
          } else {
            out[[.y]] <- NULL
          }
          return(out)
        }
      )
  } else {
    # cols is not NULL
    if (length(cols) < 2) {
      stop("Specify more than one column to compare.")
    } else if (length(cols) == 2) {
      out <- mismatch_2cols(df, ref, cols[1], cols[2])
    } else {
      combinat_cols <- t(combn(cols, 2))

      # For each combination
      out[[.y]] <- seq(nrow(combinat_cols)) %>%
        map(
          function(x) mismatch_2cols(
            df, ref, combinat_cols[x, 1], combinat_cols[x, 2]
          )
        )
    }
  }

  if (length(out) == 1 & length(out[[1]]) == 1 & length(out[[1]][[1]]) == 1) {
    return(out[[1]][[1]][[1]])
  } else {
    return(out)
  }
}

mismatch_2cols <- function(df, ref, x, y) {
  df %>%
    select(tidyselect::any_of(c(ref, x, y))) %>%
    filter(
      !is.na(!!as.name(x)) & is.na(!!as.name(y)) |
        !is.na(!!as.name(y)) & is.na(!!as.name(x)) |
        !!as.name(x) != !!as.name(y)
    ) %>%
    mutate(across(everything(), ~ gsub("[^[:alnum:]]", "", .x))) %>%
    rowwise() %>%
    filter(
      !is.na(!!as.name(x)) & is.na(!!as.name(y)) |
        !is.na(!!as.name(y)) & is.na(!!as.name(x)) |
        !grepl(!!as.name(x), !!as.name(y)) &
          !grepl(!!as.name(y), !!as.name(x))
    )
}
