#' prevR themes for ggplot2
#' 
#' Two custom themes for ggplot2 graphs, hiding axis.
#' 
#' @param base_size base font size
#' @importFrom ggplot2 '%+replace%'
#' 
#' @export

theme_prevR <- function (base_size = 12) {
  ggplot2::'%+replace%'(theme_grey(base_size),
    theme(
      axis.title        = element_blank(),
      axis.text         = element_blank(),
      axis.ticks.length = unit(0, "cm"),
      plot.margin       = unit(c(0, 0, 0, 0), "lines"),
      complete          = TRUE
    )
  )
}

#' @export
#' @rdname theme_prevR

theme_prevR_light <- function (base_size = 12) {
  '%+replace%'(theme_grey(base_size),
    theme(
      axis.title        = element_blank(),
      axis.text         = element_blank(),
      panel.background  = element_blank(),
      panel.grid        = element_blank(),
      axis.ticks.length = unit(0, "cm"),
      plot.margin       = unit(c(0, 0, 0, 0), "lines"),
      complete          = TRUE
    )
  )
}
