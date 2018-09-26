#' Squishing Y-Axis
#'
#' This function takes an interval and outputs a trans object
#' that "squishes" the y-axis where it may be uninformative.
#' This was taken from the following link:
#' https://stackoverflow.com/questions/35511951/r-ggplot2-collapse-or-remove-segment-of-y-axis-from-scatter-plot
#' @param from Lower bound of the interval to be squished
#' @param to Upper bound of the interval to be squished
#' @param factor factor to be used in squishing-interval
#' @return A trans object that can be used to ggplot
#' @import scales
#' @export

squish_trans <- function(from, to, factor) {
  trans <- function(x) {
    # get indices for the relevant regions
    isq <- x > from & x < to
    ito <- x >= to
    # apply transformation
    x[isq] <- from + (x[isq] - from) / factor
    x[ito] <- from + (to - from) / factor + (x[ito] - to)
    return(x)
  }
  inv <- function(x) {
    # get indices for the relevant regions
    isq <- x > from & x < from + (to - from) / factor
    ito <- x >= from + (to - from) / factor
    # apply transformation
    x[isq] <- from + (x[isq] - from) * factor
    x[ito] <- to + (x[ito] - (from + (to - from) / factor))
    return(x)
  }
  # return the transformation
  return(scales::trans_new("squished", trans, inv))
}
