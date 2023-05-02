#' prevR themes for ggplot2
#'
#' Two custom themes for ggplot2 graphs, hiding axis.
#'
#' @param base_size base font size
#' @importFrom ggplot2 '%+replace%' theme theme_grey element_blank unit
#'
#' @export

theme_prevR <- function(base_size = 12) {
  ggplot2::"%+replace%"(
    ggplot2::theme_grey(base_size),
    ggplot2::theme(
      axis.title        = ggplot2::element_blank(),
      axis.text         = ggplot2::element_blank(),
      axis.ticks.length = ggplot2::unit(0, "cm"),
      plot.margin       = ggplot2::unit(c(0, 0, 0, 0), "lines"),
      complete          = TRUE
    )
  )
}

#' @export
#' @rdname theme_prevR
theme_prevR_light <- function(base_size = 12) {
  ggplot2::"%+replace%"(
    ggplot2::theme_grey(base_size),
    ggplot2::theme(
      axis.title        = ggplot2::element_blank(),
      axis.text         = ggplot2::element_blank(),
      panel.background  = ggplot2::element_blank(),
      panel.grid        = ggplot2::element_blank(),
      axis.ticks.length = ggplot2::unit(0, "cm"),
      plot.margin       = ggplot2::unit(c(0, 0, 0, 0), "lines"),
      complete          = TRUE
    )
  )
}
