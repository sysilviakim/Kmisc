#' Transpose A Dataframe and Turn First Row into Column Names
#'
#' Transpose an existing dataframe, and make sure there's an option to use the
#' first resulting row as the column names.
#' Arguments for `as_tibble` are allows as ellipsis.
#'
#' @importFrom dplyr "%>%"
#' @importFrom dplyr as_tibble
#' @importFrom dplyr mutate_all
#' @importFrom readr parse_guess
#'
#' @param x The object(s) with row IDs to be converted to dataframes
#' @param col Whether to take the resulting first row and make it column names.
#' Defaults to TRUE.
#' @param row Column to keep the original column names as another column, if
#' specified. Defaults to NULL.
#' @param parse Whether to parse column types, because numeric columns would be
#' otherwise converted to character. Default to TRUE.
#' @param name_repair To be used within `as_tibble` as it otherwise complains.
#' Defaults to "unique".
#' @param ... Other arguments for `as_tibble`.
#'
#' @examples
#' ## FiveThirtyEight 2020 general election prediction: 0 = Trump, 1 = Biden
#' df <- structure(
#'   list(
#'     name = "FiveThirtyEight", al = 0, ak = 0,
#'     az = 1, ar = 0, ca = 1, co = 1, ct = 1, dc = 1, de = 1, fl = 1, ga = 1,
#'     hi = 1, id = 0, il = 1, `in` = 0, ia = 0, ks = 0, ky = 0,
#'     la = 0, me = 1, md = 1, ma = 1, mi = 1, mn = 1, ms = 0, mo = 0,
#'     mt = 0, ne = 0.3, nv = 1, nh = 1, nj = 1, nm = 1,
#'     ny = 1, nc = 1, nd = 0, oh = 0, ok = 0, or = 1, pa = 1, ri = 1,
#'     sc = 0, sd = 0, tn = 0, tx = 0, ut = 0, vt = 1, va = 1, wa = 1,
#'     wv = 0, wi = 1, wy = 0
#'   ),
#'   row.names = c(NA, -1L),
#'   class = c("data.frame")
#' )
#' transpose(df, row = "state")
#'
#' @export

transpose <- function(x,
                      col = TRUE,
                      row = NULL,
                      parse = TRUE,
                      name_repair = "unique",
                      ...) {
  ## Counter error no visible binding for global variable error
  . <- NA
  out <- x %>%
    t() %>%
    as_tibble(rownames = row, .name_repair = name_repair, ...)
  if (col) {
    out <- out %>%
      `colnames<-`(.[1, ]) %>%
      .[-1, ]
  }
  if (parse) {
    out <- out %>% mutate_all(parse_guess)
  }
  return(out)
}
