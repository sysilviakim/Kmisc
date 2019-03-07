#' Create Census Geocoder Input Files and Curl Submission File
#'
#' This function takes a dataframe and creates a series of 10,000 line input
#' text files to be uploaded to the Census Geocoder for batch geocoding.
#' It also creates a text file in which curl commands are written.
#'
#' @importFrom utils read.table
#' @importFrom stringr str_split
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#' @importFrom dplyr arrange
#' @importFrom dplyr select
#' @importFrom dplyr anti_join
#' @importFrom dplyr left_join
#' @importFrom dplyr "%>%"
#' @importFrom magrittr "%<>%"
#' @importFrom assertthat assert_that
#'
#' @param df Input dataframe.
#' @param output_path File path for output text files.
#' @param output_prefix File prefix for output text files.
#' @param benchmark The benchmark selection for Census Geocoder.
#' @param vintage The vintage selection for Census Geocoder.
#'
#' @export

geocoder_curl_output <- function(df,
                                 output_path = ".",
                                 output_prefix = NULL,
                                 benchmark = "Public_AR_Census2010",
                                 vintage = "Census2010_Census2010") {
  tiger <- state_fips <- county_fips <- census_tract <- census_block <- NULL
  benchmark_abbr <- tolower(tail(unlist(str_split(benchmark, "_")), 1))
  if (is.null(output_prefix)) {
    output_prefix <- paste0("geobatch_", benchmark_abbr, "_")
  }
  for (i in seq(ceiling(nrow(df) / 10000))) {
    temp <- read.table(
      file = file.path(output_path, paste0(output_prefix, i, ".txt")),
      sep = ",", row.names = NULL, stringsAsFactors = FALSE,
      na.strings = "", fill = TRUE, comment.char = "",
      col.names = c(
        "row", "address_input", "match", "match_detail", "address_output",
        "latlon", "tiger", "side", "state_fips", "county_fips",
        "census_tract", "census_block"
      ),
      colClasses = "character"
    )
    if (i == 1) {
      out <- temp
    } else {
      out <- bind_rows(out, temp)
    }
  }
  out %<>%
    dplyr::mutate(
      row = as.integer(row),
      tiger = as.integer(tiger),
      state_fips = as.integer(state_fips),
      county_fips = as.integer(county_fips),
      census_tract = as.integer(census_tract),
      census_block = as.integer(census_block)
    ) %>%
    filter(!is.na(row))
  ### 1     ID          ID from original address list
  ### 2     Address1            Address from original address list
  ### 3     Matching Result 1   Results indicating whether or not there was a match for the address (Match, tie, no match)
  ### 4     Matching Result 2   Results indicating if the match is exact or not (Exact, non-exact)
  ### 5     Address2            Address the original address matches to
  ### 6     Latitude, Longitude Interpolated latitude and longitude for the address
  ### 7     TIGER/Line ID       Unique ID for the edge the address falls on in the MAF/TIGER database
  ### 8     Side        Side of the street address in on (L for left and R for right)
  ### 9     State       State FIPS Code
  ### 10    County              County FIPS Code
  ### 11    Census Tract        Census Tract Code
  ### 12    Census Block        Census Block Code
  assert_that(nrow(anti_join(df, out)) == 0)
  print("Match rate is as follows: ")
  print(round(prop.table(table(out$match)) * 100, digits = 1))
  x <- list()
  x[[paste0("matched_", benchmark_abbr)]] <- left_join(df, out) %>%
    filter(!is.na(match) & match == "Match") %>%
    mutate(
      benchmark = benchmark,
      vintage = vintage,
      date = file.info(
        file.path(output_path, paste0(output_prefix, i, ".txt"))
      )$ctime
    )
  x[[paste0("unmatched_", benchmark_abbr)]] <- left_join(df, out) %>%
    filter(!(!is.na(match) & match == "Match"))
  assert_that(nrow(x[[1]]) + nrow(x[[2]]) == nrow(df))
  return(x)
}
