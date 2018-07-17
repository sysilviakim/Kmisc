#' No Title Plots
#'
#' This is a function that strips the plot of all its titles,
#' including axis titles.
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
        plot.title   = ggplot2::element_blank()
        legend.title = ggplot2::element_blank()
      )
  )
}
