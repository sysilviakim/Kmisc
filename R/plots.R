#' Graphics Option: PNG Presentations, Default Option
#'
#' This function implements my *personal* favorite setup for beamer pngs.
#' This does not currenty contain details on colors.

#' @import fontcm
#' @importFrom extrafont loadfonts
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
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(diamonds, aes(depth, colour = cut)) +
#'   geom_density()
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
    grDevices::windowsFonts(font = grDevices::windowsFont("OfficinaSanITCBoo"))
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
#' p <- ggplot(diamonds, aes(depth, colour = cut)) +
#'   geom_density()
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
#' p <- ggplot(diamonds, aes(depth, colour = cut)) +
#'   geom_density() +
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
#' p <- ggplot(diamonds, aes(depth, colour = cut)) +
#'   geom_density()
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
#'
#' @param ... A series of plots to share a common legend
#' @param list A list of plots to share a common legend.
#' Defaults to NULL. If list is specified, ellipsis argument is ignored.
#' @param ncol Number of columns of the arranged plots.
#' Defaults to NULL, in which case it is the length of the list.
#' @param nrow Number of rows of the arranged plots
#' @param position Position of the legend
#' @param legend If legend is supplied from outside. Defaults to NULL.
#' @param legend_plot If plot to pull the legend from is supplied from outside.
#' Defaults to NULL.
#'
#' @export

grid_arrange_shared_legend <- function(...,
                                       list = NULL,
                                       ncol = NULL,
                                       nrow = 1,
                                       position = c("bottom", "right"),
                                       legend = NULL,
                                       legend_plot = NULL) {
  if (!is.null(list)) {
    plots <- list
  } else {
    plots <- list(...)
  }

  if (is.null(ncol)) {
    ncol <- length(plots)
  }

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
    "bottom" = gridExtra::arrangeGrob(
      do.call(gridExtra::arrangeGrob, gl), legend,
      ncol = 1,
      heights = unit.c(unit(1, "npc") - lheight, lheight)
    ),
    "right" = gridExtra::arrangeGrob(
      do.call(gridExtra::arrangeGrob, gl), legend,
      ncol = 2,
      widths = unit.c(unit(1, "npc") - lwidth, lwidth)
    )
  )
  grid.newpage()
  grid.draw(combined)
  invisible(combined)
}

#' Common-Legend Arrangement of Plots
#'
#' This function creates a common legend for the given pots to share
#' This was taken from the following link:
#' https://github.com/tidyverse/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
#'
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 ggtitle
#' @importFrom grid textGrob
#' @importFrom grid gpar
#' @importFrom gridExtra grid.arrange
#' @import fontcm
#' @importFrom extrafont font_install
#' @importFrom extrafont loadfonts
#'
#' @param ... A series of plots to share common axes.
#' @param list A list of plots to share common axes.
#' Defaults to NULL. If list is specified, ellipsis argument is ignored.
#' @param ncol Number of columns of the arranged plots.
#' Defaults to NULL, in which case it is the length of the list.
#' @param nrow Number of rows of the arranged plots
#' @param widths Width to control the plot size.
#' @param title Plot-specific titles. Defaults to NULL.
#' @param xlab Common x-axis title to plots. Defaults to first plot's.
#' @param ylab Common y-axis title to plots. Defaults to first plot's.
#' @param fontfamily Font family to be used in common axes labels.
#' Defaults to CM Roman.
#' @param pdf_default Whether to apply pdf_default, which can reset `theme`.
#'
#' @export

grid_arrange_shared_axes <- function(...,
                                     list = NULL,
                                     ncol = NULL,
                                     nrow = 1,
                                     widths = NULL,
                                     title = NULL,
                                     xlab = NULL,
                                     ylab = NULL,
                                     fontfamily = "CM Roman",
                                     pdf_default = TRUE) {
  if (!is.null(list)) {
    plots <- list
  } else {
    plots <- list(...)
  }

  if (is.null(ncol)) {
    ncol <- min(length(plots), length(plots) / nrow)
  }
  if (is.null(xlab)) {
    xlab <- plots[[1]]$labels$x
  }
  if (is.null(ylab)) {
    ylab <- plots[[1]]$labels$y
  }
  if (is.null(widths)) {
    if (ncol == 2) {
      widths <- c(6 / 11, 5 / 11)
    } else if (ncol == 1) {
      widths <- 1
    } else {
      widths <- c(6 / (ncol * 5 + 1), rep(5 / (ncol * 5 + 1), ncol - 1))
    }
  }

  lplots <- length(plots)

  if (pdf_default) {
    plots <- plots %>%
      map(
        ~ pdf_default(.x) +
          theme(
            axis.title.x = element_blank(),
            axis.title.y = element_blank()
          )
      )
  } else {
    plots <- plots %>%
      map(
        ~ .x +
          theme(
            axis.title.x = element_blank(),
            axis.title.y = element_blank()
          )
      )
  }

  if (nrow == 1) {
    ## Edge case: nrow == 1
    for (i in seq(2, lplots)) {
      plots[[i]] <- plots[[i]] +
        theme(
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()
        ) +
        ggtitle(title[i])
    }
    plots[[1]] <- plots[[1]] +
      ggtitle(title[1])
  } else if (ncol == 1) {
    ## Edge case: ncol == 1
    for (i in seq(1, lplots - 1)) {
      plots[[i]] <- plots[[i]] +
        theme(
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()
        ) +
        ggtitle(title[i])
    }
    plots[[lplots]] <- plots[[lplots]] +
      theme(axis.title.x = element_blank()) +
      ggtitle(title[lplots])
  } else {
    ## All else normal rectangular cases
    for (i in seq(1, lplots - ncol, by = ncol)) {
      ## Leftmost column: delete x text except for last row
      plots[[i]] <- plots[[i]] +
        theme(
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()
        ) +
        ggtitle(title[i])
    }
    for (i in setdiff(
      seq(1, lplots),
      c(seq(1, lplots - 1, by = ncol), seq(lplots - ncol + 1, lplots))
    )) {
      ## Inner rectangle: delete everything
      plots[[i]] <- plots[[i]] +
        theme(
          axis.title = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()
        ) +
        ggtitle(title[i])
    }
    for (i in c(lplots - ncol + 1)) {
      ## Bottom leftmost piece:
      plots[[i]] <- plots[[i]] +
        theme(axis.title = element_blank()) +
        ggtitle(title[i])
    }
    for (i in seq(lplots - ncol + 2, lplots)) {
      plots[[i]] <- plots[[i]] +
        theme(
          axis.title = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()
        ) +
        ggtitle(title[i])
    }
  }
  ## https://stackoverflow.com/questions/10706753/
  do.call(
    "grid.arrange",
    list(
      grobs = plots,
      widths = widths,
      nrow = nrow,
      ncol = ncol,
      bottom = textGrob(xlab, gp = gpar(fontfamily = fontfamily)),
      left = textGrob(ylab, rot = 90, gp = gpar(fontfamily = fontfamily))
    )
  )
}

#' Visualization of caret::varImp
#'
#' This function takes a caret package training output and visualizes
#' the variable importance.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 scale_x_discrete
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 scale_color_continuous
#'
#' @param x Model output from caret.
#' @param filename File name to export the PDF.
#' @param labels If separate labels are provided for features.
#' Defaults to NULL.
#' @param font Which font to use. If not specified, defaults to
#' Kievit Offc Pro.
#' @param n_max Number of top variables to show. Defaults to 10.
#' @param size Size of the strip.text.x element from `ggplot2::theme`.
#' Defaults to 12.
#' @param width Width of the PDF export. Defaults to 8.
#' @param height Height of the PDF export. Defaults to 4.
#' @param seed Randomization seed. Defaults to 100.
#'
#' @export

pdf_varimp <- function(x, filename, labels = NULL, font = NULL, n_max = 10,
                       size = 12, width = 8, height = 4, seed = 100) {
  Overall <- NULL
  if (is.null(font)) font <- "Kievit Offc Pro"
  set.seed(seed)
  temp <- caret::varImp(x)
  ## Only actually important variables. Not zeros.
  p <- ggplot(
    temp,
    top = min(n_max, nrow(temp$importance %>% filter(Overall > 0)))
  )
  if (!is.null(labels)) {
    p <- p + scale_x_discrete("Feature", labels = labels)
  }
  grDevices::pdf(filename, width = width, height = height)
  print(p_font(pdf_default(p), font = font, size = size))
  grDevices::dev.off()
}

p_font <- function(p, font, size) {
  p +
    theme(
      plot.title = element_text(family = font),
      text = element_text(family = font),
      axis.text.x = element_text(family = font),
      axis.text.y = element_text(family = font),
      legend.text = element_text(family = font),
      strip.text.x = element_text(size = size)
    ) +
    scale_color_continuous()
}

