#' Spatial filter
#'
#' This function forces points of an object of class
#' `[`sf] located outside
#' the limits defined by an object of class [sp::SpatialPolygons-class]
#' to \code{NA}.
#'
#' @param x object of class [sf::sf]
#' @param y object of class [sf::sf]
#'
#' @details
#' The function try to apply `sf::st_filter()`. In case it fails,
#' it will try to rebuild `y` according to spherical geometry
#' (see [sf::st_as_s2()]) before filtering. If it still fail, it will return
#' `x` unfiltered.
#' @return Return `x` filtered by `y`
#' @seealso [sf::st_filter()].
#' @keywords manip spatial
#' @importFrom sf st_filter st_as_s2
#' @export

st_filter_prevR <- function(x, y) {
  res <- try(sf::st_filter(x, y), silent = TRUE)
  if (!inherits(res, "try-error"))
    return(res)

  y <- sf::st_as_s2(y, rebuild = TRUE)
  y <- sf::st_as_sf(y)

  res <- try(sf::st_filter(x, y), silent = TRUE)
  if (!inherits(res, "try-error"))
    return(res)

  x
}
