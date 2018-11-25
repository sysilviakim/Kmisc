#' Common-Legend Arrangement of Plots
#'
#' This function creates a common legend for the given pots to share
#' This was taken from the following link:
#' https://github.com/tidyverse/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs

#' @import ggplot2
#' @import grid
#' @importFrom gridExtra arrangeGrob
#' @param ... A series of plots to share a common legend
#' @param ncol Number of columns of the arranged plots
#' @param nrow Number of rows of the arranged plots
#' @param position Position of the legend
#' @param legend If legend is supplied from outside. Defaults to NULL.
#' @export

grid_arrange_shared_legend <- function(...,
                                       ncol = length(list(...)),
                                       nrow = 1,
                                       position = c("bottom", "right"),
                                       legend = NULL) {
  plots <- list(...)
  position <- match.arg(position)
  g <-
    ggplot2::ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  if (is.null(legend)) {
    legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  }
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position = "none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)
  combined <- switch(
    position,
    "bottom" = gridExtra::arrangeGrob(
      do.call(gridExtra::arrangeGrob, gl), legend,
      ncol = 1,
      heights = grid::unit.c(unit(1, "npc") - lheight, lheight)
    ),
    "right" = gridExtra::arrangeGrob(
      do.call(gridExtra::arrangeGrob, gl), legend,
      ncol = 2,
      widths = grid::unit.c(unit(1, "npc") - lwidth, lwidth)
    )
  )
  grid::grid.newpage()
  grid::grid.draw(combined)
  # return gtable invisibly
  invisible(combined)
}
