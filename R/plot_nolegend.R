#' No Legend Plots
#'
#' This is a function that strips the plot of its legend.

#' @import ggplot2
#' @param p The plot that we will strip its legend of.
#' @examples
#' library(ggplot2)
#' p <- ggplot(diamonds, aes(depth, colour = cut)) + geom_density()
#' plot_nolegend(p)
#' @export

plot_nolegend <- function(p) {
  return(p + ggplot2::theme(legend.position = "none"))
}

