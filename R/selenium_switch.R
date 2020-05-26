#' Switch Window Handles in RSelenium
#'
#' This function is from [RSelenium GitHub Issue #143](https://github.com/ropensci/RSelenium/issues/143):
#' John Harrison's answer.
#' This helps switch between window handles when using RSelenium.
#' Call such as `remDr$getWindowHandles()[[1]]` and so on.

#' @import RSelenium
#'
#' @param remDr `remoteDriver` object created from RSelenium `rsDriver`.
#' @param windowID ID of the window handle to switch to.
#'
#' @export

selenium_switch <- function(remDr, windowID) {
  qpath <- sprintf(
    "%s/session/%s/window", remDr$serverURL,
    remDr$sessionInfo[["id"]]
  )
  remDr$queryRD(qpath, "POST", qdata = list(handle = windowID))
}
