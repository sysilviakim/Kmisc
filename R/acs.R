#' ACS Processing/Merging Functions
#'
#' @importFrom dplyr "%>%"
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr rowwise
#' @importFrom dplyr mutate_if
#' @importFrom dplyr select_if
#' @importFrom readr read_csv
#' @importFrom readr cols
#' @importFrom magrittr set_names

acs_geoid_crosswalk <- function(df,
                                acs_tract = "acs_tract",
                                acs_block = "acs_block",
                                state_fips = "state_fips",
                                county_fips = "county_fips",
                                census_tract = "census_tract",
                                census_block = "census_block") {
  out <- df %>%
    mutate(
      !!acs_tract := ifelse(
        !is.na(!!as.name(census_tract)),
        paste0(
          str_pad(!!as.name(state_fips), width = 2, pad = "0"),
          str_pad(!!as.name(county_fips), width = 3, pad = "0"),
          str_pad(!!as.name(census_tract), width = 6, pad = "0")
        ), NA
      ),
      !!acs_block := ifelse(
        !is.na(!!as.name(census_block)),
        paste0(!!as.name(acs_tract), substr(!!as.name(census_block), 1, 1)),
        NA
      )
    )
  return(out)
}

acs_income_readr <- function(file, upper_limit = "250000") {
  out <- read_csv(
    file, col_names = FALSE, col_types = cols(.default = "c"),
    skip = 2, quote = "", na = ""
  ) %>%
    select(
      geoid = X2,
      est = !!paste0("X", dim(.)[2] - 1),
      moe = !!paste0("X", dim(.)[2])
    ) %>%
    rowwise() %>%
    mutate(
      ## Truncated amount
      est = ifelse(grepl("\\+", moe), upper_limit, est),
      est = as.numeric(est),
      moe = as.numeric(moe)
    )
}

acs_edu_readr <- function(file) {
  out <- read_csv(
    file,
    col_names = FALSE, col_types = cols(.default = "c"),
    skip = 2, quote = "", na = ""
  ) %>%
    select(-X1) %>%
    mutate_if(is.character, as.numeric) %>%
    select_if(function(x) all(!is.na(x))) %>%
    set_names(c(
      "geoid",
      c(t(outer(
        c(
          "total", "none", "nursery", "kindergarten", paste0("g", seq(12)),
          "hsgrad", "ged", "c1", "c2", "assoc", "ba", "ma", "prof", "phd"
        ),
        c("est", "moe"),
        FUN = paste, sep = "_"
      )))
    )) %>%
    ## We don't need everything.
    mutate(
      college_prop = (ba_est + ma_est + prof_est + phd_est) / total_est,
      geoid = str_pad(geoid, width = 12, pad = "0", side = "left")
    ) %>%
    select(-matches("none|nursery|kindergarten")) %>%
    select(-matches(paste0("g", seq(12)) %>% paste(collapse = "|")))
}
