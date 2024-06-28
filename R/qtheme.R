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
  ggthemes::theme_par(...) +
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
  ggpubr::theme_pubr(...) +
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

#' A funtion to save ggplot project in fixed style
#' @author qinti
#' @param name
#' filename of pdf or png
#' @param w
#' the width of plot
#' @param h
#' the height of plot
#' @return No return value. This function is called for its side effects.
#' @export
#'
#' @examples
#' library(ggplot2)
#' data <- data.frame(x = 1:10, y = 2:11)
#' ggplot(data, mapping = aes(x = x, y = y)) +
#'   geom_point()
#' tmpfile <- tempfile(fileext = ".pdf")
#' ttsave(tmpfile, 5.5, 5.5)
ttsave <- function(name, w, h) {
  if (!is.character(name) || length(name) != 1) {
    stop("The 'name' parameter must be a single character string.")
  }
  if (!is.numeric(w) || length(w) != 1) {
    stop("The 'w' parameter must be a single numeric value.")
  }
  if (!is.numeric(h) || length(h) != 1) {
    stop("The 'h' parameter must be a single numeric value.")
  }
  ggplot2::ggsave(filename = name, width = w, height = h, dpi = 600, units = "cm")
}
