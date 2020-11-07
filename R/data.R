#' State-level Geographic Indicators
#'
#' A dataset containing the geographic indicators for U.S. states,
#' including the District of Columbia.
#'
#' @format A dataframe with 51 rows and 5 variables:
#' \describe{
#'   \item{stname}{State names, in lowercase}
#'   \item{stabb}{State abbreviations}
#'   \item{stfips}{FIPS code}
#'   \item{south_all}{Southern states}
#'   \item{south_conf}{Southern states (11 confederate)}
#'   \item{region}{Region classification by Census Bureau}
#'   \item{division}{Division classification by Census Bureau}
#' }
"fips"

#' Examples of U.S. Residential Addresses
#'
#' A dataset containing 1,000 example residential addresses of the U.S.,
#' sampled from the Federal Election Commission receipts, Schedule A,
#' individuals, within the 2018 Election Cycle. 48 states and DC addresses
#' present, with MS and ND obs missing, presumably due to small sample.
#' All entries are stripped of non-ASCII, punctuations, extra spaces or tabs,
#' and all are lowercased.
#'
#' @format A dataframe with 1,000 rows and 5 variables:
#' \describe{
#'   \item{street}{Street address}
#'   \item{city}{City}
#'   \item{state}{State}
#'   \item{zip}{5-digit zip code}
#'   \item{address_input}{First four variables pasted, separated by comma}
#' }
"ex_addr"

#' State Names and Abbreviations Dataframe
#'
#' A dataset that puts state.abb and state.names together into a single
#' dataframe, along with DC.
#'
#' @format A dataframe with 51 rows and 2 variables:
#' \describe{
#'   \item{abb}{State Abbreviations}
#'   \item{names}{State Names}
#' }
"state_df"

