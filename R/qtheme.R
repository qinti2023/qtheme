#' A simple-looking ggplot2 theme for publication: some adjustments from the theme_par function.
#' @author qinti
#' @param ...
#' parameters passed to theme_par function
#' @param bg
#' color for background
#' @return
#' ggplot2 theme object
#' @export
#' @examples
#' library(ggpubr)
#' data.frame(x = 1:10, y = 2:11) %>%
#'   ggscatter(x = "x", y = "y", color = "x") +
#'   theme_q()
theme_q <- function(..., bg = "white") {
  ggthemes::theme_par(base_size = 8) +
    ggplot2::theme(
      rect = ggplot2::element_rect(fill = bg),
      plot.margin = ggplot2::unit(rep(0.5, 4), "lines"),
      panel.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(fill = "transparent", color = "black", linewidth = ggplot2::unit(0.2325, "pt")), # 边框线宽度为 0.5 pt
      panel.grid = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(color = "black", vjust = 0.1),
      axis.ticks.length = ggplot2::unit(0.15, "lines"),
      axis.ticks = ggplot2::element_line(color = "black", linewidth = ggplot2::unit(0.2325, "pt")),
      legend.title = ggplot2::element_blank(),
      legend.key = ggplot2::element_rect(fill = "transparent", color = "transparent")
    )
}

#' A simple-looking ggplot2 theme for publication: some adjustments from the theme_pubr function.
#' @author qinti
#' @param ...
#' parameters passed to theme_pubr function
#' @param bg
#' color for background
#' @return
#' ggplot2 theme object
#' @export
#' @examples
#' library(ggpubr)
#' data.frame(x = 1:10, y = 2:11) %>%
#'   ggscatter(x = "x", y = "y", color = "x") +
#'   theme_t()
theme_t <- function(..., bg = "white") {
  ggpubr::theme_pubr(base_size = 8) +
    ggplot2::theme(
      rect = ggplot2::element_rect(fill = bg),
      panel.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(color = "black", linewidth = ggplot2::unit(0.2325, "pt")),
      axis.title = ggplot2::element_text(color = "black", vjust = 0.1),
      axis.ticks.length = ggplot2::unit(0.15, "lines"),
      axis.ticks = ggplot2::element_line(color = "black", linewidth = ggplot2::unit(0.2325, "pt")),
      legend.title = ggplot2::element_blank()
    )
}
