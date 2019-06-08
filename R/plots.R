#' Graphics Option: PNG Presentations, Default Option
#'
#' This function implements my *personal* favorite setup for beamer pngs.
#' This does not currenty contain details on colors.

#' @import fontcm
#' @importFrom extrafont loadfonts
#' @import grDevices
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_text
#' @importFrom ggthemes theme_economist
#' @importFrom ggthemes scale_colour_economist
#'
#' @param p The input ggplot2 object
#' @param font Font for the graphics. Default is NULL.
#' @param legend.position Legend position passed to ggplot2. Default is bottom.
#' @param legend.direction Legend direction passed to ggplot.
#' Default is horizontal.
#' @keywords plot
#' @examples
#' library(ggplot2)
#' p <- ggplot(diamonds, aes(depth, colour = cut)) + geom_density()
#' png_default(p)
#' @export

png_default <- function(p,
                        font = NULL,
                        legend.position = "bottom",
                        legend.direction = "horizontal") {
  loadfonts()
  if (Sys.info()["sysname"] == "Windows") {
    loadfonts(device = "win")
  }
  if (is.null(font)) {
    font <- "sans"
  } else if (
    grepl(tolower("OfficinaSans|Economist"), tolower(font)) &
      Sys.info()["sysname"] == "Windows"
  ) {
    windowsFonts(font = windowsFont("OfficinaSanITCBoo"))
    font <- "font"
  }
  p <- p +
    theme_economist() +
    scale_colour_economist() +
    theme(
      plot.title = element_text(family = font),
      text = element_text(family = font),
      axis.text.x = element_text(family = font),
      axis.text.y = element_text(family = font),
      legend.text = element_text(family = font),
      legend.position = legend.position,
      legend.direction = legend.direction,
      legend.title = element_blank()
    )
  return(p)
}

#' Graphics Option: PDF Articles, Default Option
#'
#' This function implements my *personal* favorite setup for article pdfs.
#' This does not currenty contain details on colors.
#' Note that the CM Roman font will not show immediately when called in the
#' Rstudio plot pane, but will be correctly input in the resulting pdf file.

#' @import fontcm
#' @import grDevices
#' @importFrom extrafont font_install
#' @importFrom extrafont loadfonts
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 theme_bw
#' @param p The input ggplot2 object
#' @param CMRoman Font for the graphics. Default is CM Roman.
#' @keywords plot
#' @examples
#' library(ggplot2)
#' p <- ggplot(diamonds, aes(depth, colour = cut)) + geom_density()
#' pdf_default(p)
#' @export

pdf_default <- function(p, CMRoman = NULL) {
  if (is.null(CMRoman)) {
    CMRoman <- "CM Roman"
    requireNamespace("fontcm")
    font_install("fontcm")
    loadfonts()
    requireNamespace("ggplot2")
  }
  p <- p +
    theme_bw() +
    theme(
      plot.title = element_text(family = CMRoman),
      text = element_text(family = CMRoman),
      axis.text.x = element_text(family = CMRoman),
      axis.text.y = element_text(family = CMRoman),
      legend.text = element_text(family = CMRoman)
    )
  return(p)
}

#' No Title Plots
#'
#' This is a function that strips the plot of all its titles,
#' including axis titles and legend titles.
#'
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_blank
#'
#' @param p The plot that we will strip its titles of.
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(diamonds, aes(depth, colour = cut)) + geom_density() +
#'   ggtitle("Diamonds")
#' plot_notitle(p)
#' @export

plot_notitle <- function(p) {
  return(
    p +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_blank(),
        legend.title = element_blank()
      )
  )
}

#' No Legend Plots
#'
#' This is a function that strips the plot of its legend.
#'
#' @importFrom ggplot2 theme
#'
#' @param p The plot that we will strip its legend of.
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(diamonds, aes(depth, colour = cut)) + geom_density()
#' plot_nolegend(p)
#' @export

plot_nolegend <- function(p) {
  return(p + theme(legend.position = "none"))
}

#' No Grid/Axis Plots
#'
#' This is a function that strips the plot of all its grid and axes elements.
#'
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_blank
#'
#' @param p The plot that we will strip its grids/axes of.
#' @export

plot_nogrid <- function(p) {
  return(
    p +
      theme(
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

#' Common-Legend Arrangement of Plots
#'
#' This function creates a common legend for the given pots to share
#' This was taken from the following link:
#' https://github.com/tidyverse/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
#'
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 ggplotGrob
#' @importFrom grid grid.newpage
#' @importFrom grid grid.draw
#' @importFrom grid unit
#' @importFrom grid unit.c
#' @importFrom gridExtra arrangeGrob
#'
#' @param ... A series of plots to share a common legend
#' @param ncol Number of columns of the arranged plots
#' @param nrow Number of rows of the arranged plots
#' @param position Position of the legend
#' @param legend If legend is supplied from outside. Defaults to NULL.
#' @param legend_plot If plot to pull the legend from is supplied from outside.
#' Defaults to NULL.
#'
#' @export

grid_arrange_shared_legend <- function(...,
                                       ncol = length(list(...)),
                                       nrow = 1,
                                       position = c("bottom", "right"),
                                       legend = NULL,
                                       legend_plot = NULL) {
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  if (!is.null(legend_plot)) {
    g <- ggplotGrob(legend_plot + theme(legend.position = position))$grobs
  }
  if (is.null(legend) | !is.null(legend_plot)) {
    legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  }
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position = "none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)
  combined <- switch(
    position,
    "bottom" = arrangeGrob(
      do.call(arrangeGrob, gl), legend,
      ncol = 1,
      heights = unit.c(unit(1, "npc") - lheight, lheight)
    ),
    "right" = arrangeGrob(
      do.call(arrangeGrob, gl), legend,
      ncol = 2,
      widths = unit.c(unit(1, "npc") - lwidth, lwidth)
    )
  )
  grid.newpage()
  grid.draw(combined)
  invisible(combined)
}
