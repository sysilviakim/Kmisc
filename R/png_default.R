#' Graphics Option: PNG Presentations, Default Option
#'
#' This function implements my *personal* favorite setup for beamer pngs.
#' This does not currenty contain details on colors.

#' @import fontcm
#' @import grDevices
#' @import ggplot2
#' @param p The input ggplot2 object
#' @param OfficinaSans Font for the graphics. Default is OfficinaSansITBoo.
#' @keywords plot
#' @examples
#' library(ggplot2)
#' p <- ggplot(diamonds, aes(depth, colour = cut)) + geom_density()
#' png_default(p)
#' @export

png_default <- function(p, OfficinaSans = NULL) {
  if (is.null(OfficinaSans)) {
    if (Sys.info()['sysname'] == "Windows") {
      grDevices::windowsFonts(
        OfficinaSans = grDevices::windowsFont("OfficinaSanITCBoo")
      )
      OfficinaSans = "OfficinaSans"
    } else {
      stop("Please specify a OfficinaSans to be used.")
    }
  }
  p <- p +
    ggthemes::theme_economist() +
    ggthemes::scale_colour_economist() +
    ggplot2::theme(
      plot.title  = ggplot2::element_text(family = OfficinaSans),
      text        = ggplot2::element_text(family = OfficinaSans),
      axis.text.x = ggplot2::element_text(family = OfficinaSans),
      axis.text.y = ggplot2::element_text(family = OfficinaSans),
      legend.text = ggplot2::element_text(family = OfficinaSans)
    ) +
    ggplot2::theme(
      legend.position  = "bottom",
      legend.direction = "horizontal",
      legend.title     = ggplot2::element_blank()
    )
  return(p)
}
