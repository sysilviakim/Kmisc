#' Impute Race to Categorical Variables After wru::predict_race
#'
#' If you want to pass age, sex, and party, pass it exactly as in wru
#' so that the ellipsis captures it. Detailed documentation will follow soon.
#'
#' @importFrom dplyr "%>%"
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom stringr str_pad
#' @importFrom magrittr "%<>%"
#' @importFrom wru predict_race
#'
#' @param df Dataframe prepped for wru::predict_race.
#' @param census Census object.
#' @param state State variable.
#' @param surname Surname variable.
#' @param county County variable.
#' @param tract Tract variable.
#' @param block Block variable.
#' @param id ID variable.
#' @param keep_probs Whether to keep the underlying probabilities generating
#' the classification.
#' @param ... Other arguments to be passed to wru::predict_race.
#'
#' @export

impute_race <- function(df,
                        census,
                        state = "sSitusState",
                        surname = "szNameLast",
                        county = "county_fips",
                        tract = "census_tract",
                        block = "census_block",
                        id = "lVoterUniqueID",
                        keep_probs = FALSE,
                        ...) {
  . <- race <- NULL

  if ("race" %in% names(df)) {
    stop(paste0(
      "This function will create variable race, which will overwrite ",
      "the existing variable."
    ))
  }

  race_cols <- c("whi", "bla", "his", "asi", "oth")

  prob_to_factor <- function(df, id, race_cols, keep_probs, tag) {
    out <- df %>%
      ungroup() %>%
      select(!!id, !!paste0("pred.", race_cols)) %>%
      mutate(
        race = case_when(
          pred.whi > pred.bla & pred.whi > pred.his &
            pred.whi > pred.asi & pred.whi > pred.oth ~ "white",
          pred.bla > pred.whi & pred.bla > pred.his &
            pred.bla > pred.asi & pred.bla > pred.oth ~ "black",
          pred.his > pred.bla & pred.his > pred.whi &
            pred.his > pred.asi & pred.his > pred.oth ~ "hispanic",
          pred.asi > pred.bla & pred.asi > pred.his &
            pred.asi > pred.whi & pred.asi > pred.oth ~ "asian",
          pred.oth > pred.bla & pred.oth > pred.his &
            pred.oth > pred.asi & pred.oth > pred.whi ~ "others"
        ),
        ## If race is NA, means that there is a tie.
        race = ifelse(is.na(race), "others", race)
      )
    if (keep_probs) {
      out %<>% select(
        !!id, race, !!paste0("pred.", race_cols)
      ) %>%
        rename_all(paste0, tag) %>%
        rename(!!as.name(id) := !!as.name(paste0(id, tag)))
    } else {
      out %<>% select(!!id, race) %>%
        rename_all(paste0, tag) %>%
        rename(!!as.name(id) := !!as.name(paste0(id, tag)))
    }
    return(out)
  }

  pre_wru <- df %>%
    rename(
      surname = !!as.name(surname),
      state = !!as.name(state), county = !!as.name(county),
      tract = !!as.name(tract), block = !!as.name(block)
    )

  if (!is.null(block)) {
    pre_wru %<>% mutate(
      block = str_pad(block, width = 4, pad = "0"),
      tract = str_pad(tract, width = 6, pad = "0"),
      county = str_pad(county, width = 3, pad = "0")
    )
    out_race_block <- predict_race(
      voter.file = pre_wru %>% filter(!is.na(block)),
      census.data = census, census.geo = "block",
      ...
    ) %>%
      prob_to_factor(
        ., id = id, race_cols = race_cols, keep_probs = keep_probs, tag = ".b"
      )
    df %<>% left_join(., out_race_block)
  }

  if (!is.null(tract)) {
    pre_wru %<>% mutate(
      tract = str_pad(tract, width = 6, pad = "0"),
      county = str_pad(county, width = 3, pad = "0")
    )
    out_race_tract <- predict_race(
      voter.file = pre_wru %>% filter(!is.na(tract)),
      census.data = census, census.geo = "tract",
      ...
    ) %>%
      prob_to_factor(
        ., id = id, race_cols = race_cols, keep_probs = keep_probs, tag = ".t"
      )
    df %<>% left_join(., out_race_tract)
  }

  if (!is.null(county)) {
    pre_wru %<>% mutate(county = str_pad(county, width = 3, pad = "0"))
    out_race_county <- predict_race(
      voter.file = pre_wru %>% select(-tract, -block),
      census.data = census, census.geo = "county",
      ...
    ) %>%
      prob_to_factor(
        ., id = id, race_cols = race_cols, keep_probs = keep_probs, tag = ".c"
      )
    df %<>% left_join(., out_race_county)
  }

  return(df)
}
