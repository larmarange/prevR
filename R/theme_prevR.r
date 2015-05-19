#'  prevR themes for ggplot2
#'  
#'  Two custom themes for ggplot2 graphs, hidding axis.
#'  
#'  @param base_size base font size
#'  
#'  @seealso \code{\link[ggplot2]{ggtheme}}\{\pkg{ggplot2}\}
#'  
#'  @export

theme_prevR <- function (base_size = 12) {
  requireNamespace("ggplot2", quietly = TRUE)
  requireNamespace("grid", quietly = TRUE)
  ggplot2::'%+replace%'(ggplot2::theme_grey(base_size),
    ggplot2::theme(
      axis.title        = ggplot2::element_blank(),
      axis.text         = ggplot2::element_blank(),
      axis.ticks.length = grid::unit(0, "cm"),
      axis.ticks.margin = grid::unit(0, "lines"),
      plot.margin       = grid::unit(c(0, 0, 0, 0), "lines"),
      complete          = TRUE
    )
  )
}

#' @export
#' @rdname theme_prevR

theme_prevR_light <- function (base_size = 12) {
  requireNamespace("ggplot2", quietly = TRUE)
  requireNamespace("grid", quietly = TRUE)
  ggplot2::'%+replace%'(ggplot2::theme_grey(base_size),
    ggplot2::theme(
      axis.title        = ggplot2::element_blank(),
      axis.text         = ggplot2::element_blank(),
      panel.background  = ggplot2::element_blank(),
      panel.grid        = ggplot2::element_blank(),
      axis.ticks.length = grid::unit(0, "cm"),
      axis.ticks.margin = grid::unit(0, "lines"),
      plot.margin       = grid::unit(c(0, 0, 0, 0), "lines"),
      complete          = TRUE
    )
  )
}
