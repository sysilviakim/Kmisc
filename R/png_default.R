#' Graphics Option: PNG Presentations, Default Option
#'
#' This function implements my *personal* favorite setup for beamer pngs.
#' This does not currenty contain details on colors.

#' @import fontcm
#' @import extrafont
#' @import grDevices
#' @import ggplot2
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

png_default <- function(p, font = NULL,
                        legend.position = "bottom",
                        legend.direction = "horizontal") {
  extrafont::loadfonts()
  if (Sys.info()["sysname"] == "Windows") {
    extrafont::loadfonts(device = "win")
  }
  if (is.null(font)) {
    font <- "sans"
  } else if (
    grepl(tolower("OfficinaSans|Economist"), tolower(font)) &
      Sys.info()["sysname"] == "Windows"
  ) {
    grDevices::windowsFonts(
      font = grDevices::windowsFont("OfficinaSanITCBoo")
    )
    font = "font"
  }
  p <- p +
    ggthemes::theme_economist() +
    ggthemes::scale_colour_economist() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(family = font),
      text = ggplot2::element_text(family = font),
      axis.text.x = ggplot2::element_text(family = font),
      axis.text.y = ggplot2::element_text(family = font),
      legend.text = ggplot2::element_text(family = font),
      legend.position = legend.position,
      legend.direction = legend.direction,
      legend.title = ggplot2::element_blank()
    )
  return(p)
}

