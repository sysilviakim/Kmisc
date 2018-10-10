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
