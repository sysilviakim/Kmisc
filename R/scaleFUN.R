#' Change Decimal Places in Axis Labels
#'
#' This function change the number of decimal places on axis labels.
#' This was taken from the following link:
#' https://stackoverflow.com/questions/38722202/how-do-i-change-the-number-of-decimal-places-on-axis-labels-in-ggplot2

#' @param x Object to format
#' @param format How to reformat the labels
#' @export

scaleFUN <- function(x, format = "%.1f") {
  return(sprintf(format, x))
}
