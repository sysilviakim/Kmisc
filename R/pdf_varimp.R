#' Return a Deduplicated a Dataframe
#'
#' This function takes a dataframe and returns only the deduplicated rows,
#' based on all columns.
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
#'
#'
#' @export

pdf_varimp <- function(x, filename, labels = NULL, font = NULL, n_max = 10,
                       size = 12, width = 8, height = 4) {
  Overall <- NULL
  if (is.null(font)) font <- "Kievit Offc Pro"
  set.seed(100)
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

