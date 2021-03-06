#' Show Graphically How an Attribute Changes Over Time
#'
#' This function takes a single row of a dataframe, a vector, or a list,
#' checks for changes in consecutive values over the sequence, and show at
#' which points the values change.
#'
#' @param x The sequence to be analyzed.
#' @param names Whether the sequence is named. Defaults to TRUE.
#' @param sub_str Whether to use a substring of the names of the sequence.
#' Defaults to NULL. If entered, this should be two integers in a vector.
#' @param len The length of separation in showing the changes.
#' Defaults to 2.
#' @param gap The length of separation from names to values.
#' Defaults to 15.
#'
#' @importFrom dplyr "%>%"
#'
#' @examples
#' seq_changes(c(NA, "B", rep("A", 2), rep("C", 4), "D"), gap = 1, len = 1)
#'
#' @export

seq_changes <- function(x, names = TRUE, sub_str = NULL, len = 2, gap = 15) {
  . <- NULL
  if (names == TRUE & is.null(names(x))) {
    names <- FALSE
  }
  if (!is.null(sub_str)) {
    if (
      !(length(sub_str) == 2 |
        !all(unlist(lapply(sub_str, class)) %in% c("numeric", "integer")))
    ) {
      stop("Must provide two integers for substr.")
    }
  }
  x <- unlist(x)
  cid <- data.table::rleid(x)
  temp <- lapply(unique(cid), function(x) cid == x)
  temp <- lapply(temp, function(x) min(which(x == TRUE))) %>% unlist()
  max_nchar <- max(unlist(lapply(x[temp], nchar)), na.rm = TRUE)
  if (names) {
    cat(
      lapply(
        seq(length(x[temp])),
        function(y) paste0(
            unname(x[temp][y]),
            rep(
              " ",
              max(
                max_nchar + gap -
                  ifelse(is.na(nchar(x[temp][y])), 2, nchar(x[temp][y])),
                1
              )
            ) %>%
              paste0(collapse = ""),
            ifelse(
              is.null(sub_str), names(x[temp][y]),
              substr(names(x[temp][y]), sub_str[1], sub_str[2])
            )
          )
      ) %>%
        unlist() %>%
        paste0(
          collapse = paste0(
            "\n",
            rep(" |\n", len) %>%
              paste0(collapse = "")
          )
        ) %>%
        paste0(., "\n")
    )
  } else {
    cat(
      lapply(
        seq(length(x[temp])),
        function(y) paste0(
            unname(x[temp][y]),
            rep(
              " ",
              max(
                max_nchar + gap -
                  ifelse(is.na(nchar(x[temp][y])), 2, nchar(x[temp][y])),
                1
              )
            ) %>%
              paste0(collapse = ""),
            temp[y] ## names(x[temp][y])
          )
      ) %>%
        unlist() %>%
        paste0(
          collapse = paste0(
            "\n",
            rep(" |\n", len) %>%
              paste0(collapse = "")
          )
        ) %>%
        paste0(., "\n")
    )
  }
}
