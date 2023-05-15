#' Create a spatial grid from an object of class prevR.
#'
#' This function generates a spatial rectangular grid from the slot
#' \code{boundary} of an object of class [`prevR-class`]; function used in
#' particular by the methods [kde()], [krige()] and [idw()].
#'
#' @param object object of class [prevR-class].
#' @param nb.cells number of cells on the longest side of the studied area
#'   (unused if \code{cell.size} is defined).
#' @param cell.size size of each cell (in the unit of the projection).
#'
#' @details This function generates a spatial rectangular grid, each cell
#' corresponding to the center of a square of side \code{cell.size}.
#' If \code{cell.size} is not defined, side of cells will be
#' calculated as the longest side of the slot \code{boundary} of \code{object}
#' divided by \code{nb.cells}.
#'
#' @return Object of class [sf::sfc] (simple feature geometry list column).
#' @seealso [sf::st_make_grid()]
#' @examples
#' make.grid.prevR(fdhs)
#' make.grid.prevR(fdhs, nb.cells = 200)
#' @keywords manip spatial
#' @export
make.grid.prevR <-
  function(object, nb.cells = 100, cell.size = NULL) {
    if (!is.prevR(object))
      stop("'object' must be of class prevR.")

    # calcul de la taille des cellules et
    # donc du nombre de cellules sur chaque coordonnees
    boundary <- slot(object, "boundary")
    bb <- st_bbox(boundary)
    dx <- bb["xmax"] - bb["xmin"]
    dy <- bb["ymax"] - bb["ymin"]
    if (is.null(cell.size)) {
      dxy <- max(c(dx, dy))
      cell.size <- dxy / nb.cells
      nb.cells.x <- dx %/% cell.size + 1
      nb.cells.y <- dy %/% cell.size + 1
    } else {
      nb.cells.x <- dx %/% cell.size + 1
      nb.cells.y <- dy %/% cell.size + 1
    }
    sf::st_make_grid(boundary, n = c(nb.cells.x, nb.cells.y), what = "centers")
  }
