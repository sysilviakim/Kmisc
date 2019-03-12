#' No Title Plots
#'
#' This is a function that strips the plot of all its titles,
#' including axis titles and legend titles.
#'
#' @import ggplot2
#' @param p The plot that we will strip its titles of.
#' @examples
#' library(ggplot2)
#' p <- ggplot(diamonds, aes(depth, colour = cut)) + geom_density() +
#'        ggtitle("Diamonds")
#' plot_notitle(p)
#' @export

plot_notitle <- function(p) {
  return(
    p +
      ggplot2::theme(
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        plot.title   = ggplot2::element_blank(),
        legend.title = ggplot2::element_blank()
      )
  )
}

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

#' No Grid/Axis Plots
#'
#' This is a function that strips the plot of all its grid and axes elements.
#'
#' @import ggplot2
#' @param p The plot that we will strip its grids/axes of.
#' @export

plot_nogrid <- function(p) {
  return(
    p +
      ggplot2::theme(
        panel.border = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
      )
  )
}
