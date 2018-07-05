#' Graphics Option: PDF Articles, Default Option
#'
#' This function implements my *personal* favorite setup for article pdfs.
#' This does not currenty contain details on colors.

#' @import fontcm
#' @import extrafont
#' @import grDevices
#' @import ggplot2
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
    CMRoman = "CM Roman"
    requireNamespace("fontcm")
    extrafont::font_install("fontcm")
    extrafont::loadfonts()
    requireNamespace("ggplot2")
  }
  p <- p +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title  = ggplot2::element_text(family = CMRoman),
      text        = ggplot2::element_text(family = CMRoman),
      axis.text.x = ggplot2::element_text(family = CMRoman),
      axis.text.y = ggplot2::element_text(family = CMRoman),
      legend.text = ggplot2::element_text(family = CMRoman)
    )
  return(p)
}

