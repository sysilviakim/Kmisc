#' Format Various Geocoding Outputs into Census Format
#'
#' This function takes a dataframe of geocoding outputs and wrangles it
#' into a Census Geocoder output format for comparability. The main variable
#' names are set as "address_input", "match", "match_detail", "address_output",
#' "latlon", "tiger", "side", "state_fips", "county_fips", "census_tract",
#' and "census_block".
#'
#' @importFrom dplyr "%>%"
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr filter
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom tidyr separate
#' @importFrom stringi stri_trans_general
#'
#' @param df Input dataframe.
#' @param input_type Type of output. You can specify `geocodio` or `dstk`.
#' @param longitude Variable name for longitude. Defaults to "longitude."
#' @param latitude Variable name for latitude. Defaults to "latitude"
#' @param street Variable for street address.
#' @param city Variable for city.
#' @param state Variable for state abbreviation.
#' @param zip Variable for zip code.
#' @param cutoff If specified with `accuracy`, will filter for geocoded
#' outputs with accuracy level above the cutoff. Defaults to 0.87.
#' @param accuracy Variable for accuracy.
#' @param addr_all Variable for one-line address, or street-city-state-zip
#' pasted. It will be created if not specified as existing variable.
#' @param sep Separator to use in creating `addr_all`.
#' @param file_date Geocoded date. Defaults to `Sys.time()`.
#' @param vars_keep Variables to keep besides the census format.
#'
#' @export

census_format <- function(df,
                          input_type = "n/a",
                          longitude = "longitude",
                          latitude = "latitude",
                          street = "street",
                          city = "szSitusCity",
                          state = "sSitusState",
                          zip = "sSitusZip",
                          cutoff = 0.87,
                          accuracy = NULL,
                          addr_all = NULL,
                          sep = ", ",
                          file_date = NULL,
                          vars_keep = NULL) {
  . <- street <- street_address <- locality <- region <- formatted_address1 <-
    latlon <- location.lng <- location.lat <- NULL
  out <- df
  if (input_type == "dstk") {
    accuracy <- "confidence"
    addr_all <- "full.address"
  } else if (input_type == "geocodio") {
    accuracy <- "accuracy"
    addr_all <- "query"
    longitude <- "location.lng"
    latitude <- "location.lat"
  }
  if (!is.null(cutoff) & !is.null(accuracy)) {
    if (cutoff > 1 | cutoff < 0) stop("Invalid cutoff value.")
    out <- out %>% filter(!!as.name(accuracy) > cutoff)
  }
  if (is.null(file_date)) file_date <- Sys.time()
  if (!is.null(addr_all)) {
    out <- out %>%
      mutate(address_input = !!as.name(addr_all)) %>%
      separate(
        ., !!as.name(addr_all), into = c(street, city, state, zip), sep = sep
      )
  }
  if (input_type == "dstk") {
    out <- out %>% mutate(
      address_output =
        paste(street_address, locality, region, !!as.name(zip), sep = ", ")
    )
  } else if (input_type == "geocodio") {
    out <- out %>% mutate(address_output = formatted_address1)
  }
  out <- out %>%
    addr_precensus(., vars = "address_output") %>%
    mutate(
      match = "Match",
      match_detail = case_when(
        address_input == address_output ~ "Exact",
        address_input != address_output ~ "Non_Exact"
      ),
      latlon = paste(!!as.name(longitude), !!as.name(latitude), sep = ","),
      tiger = NA, side = NA, state_fips = NA, county_fips = NA,
      census_tract = NA, census_block = NA,
      benchmark = input_type, vintage = input_type,
      date = file_date,
      !! as.name(zip) := as.numeric(!!as.name(zip)),
      latlon = ifelse(latlon == "NA,NA", NA, latlon)
    )
  if (input_type == "dstk") {
    out <- out %>% mutate(
      state_fips = case_when(
        !is.na(fips_county) ~ as.numeric(substr(fips_county, 1, 2))
      ),
      county_fips = case_when(
        !is.na(fips_county) ~ as.numeric(substr(fips_county, 3, 5))
      )
    )
  }
  out <- out %>%
    select(
      "address_input", "match", "match_detail", "address_output",
      "latlon", "tiger", "side", "state_fips", "county_fips", "census_tract",
      "census_block", "benchmark", "vintage", "date", vars_keep
    )
  return(out)
}

addr_precensus <- function(df,
                           vars,
                           create_street = FALSE,
                           house_num = "sHouseNum",
                           pre_dir = "sPreDir",
                           street_name = "szStreetName",
                           street_sfx = "sStreetSuffix",
                           post_dir = "sPostDir",
                           zip = "sSitusZip") {
  street <- NULL
  if (create_street == TRUE) {
    for (v in c(house_num, pre_dir, street_name, street_sfx, post_dir)) {
      df[[v]] <- ifelse(is.na(df[[v]]), "", clean_addr(df[[v]]))
    }
    df <- df %>%
      mutate(
        street = paste(
          !!as.name(house_num),
          !!as.name(pre_dir),
          !!as.name(street_name),
          !!as.name(street_sfx),
          !!as.name(post_dir)
        ),
        street = gsub("[ \t]{2,}", " ", trimws(street))
      )
  }
  for (v in vars) {
    df[[v]] <- gsub("^, ", "", clean_addr(df[[v]]))
    if (v == zip | v == house_num) df[[v]] <- as.numeric(df[[v]])
  }
  return(df)
}

clean_addr <- function(x, exception = ",") {
  x <- gsub(
    "[ \t]{2,}", " ",
    trimws(tolower(stri_trans_general(
      gsub(paste0("(?!", exception, ")[[:punct:]]"), " ", x, perl = TRUE),
      "latin-ascii"
    )))
  )
  return(x)
}
