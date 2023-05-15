#' @exportMethod changeproj

setGeneric(
  "changeproj",
  function(object, proj) {
    standardGeneric("changeproj")
  }
)

#' Convert map projection of a object of class prevR.
#'
#' This function converts map projection (and/or datum) used by an object of
#' class [prevR-class] into another one.
#'
#' @param object object of class [prevR-class].
#' @param proj new map projection. One of
#'   (i) character: a string accepted by GDAL, (ii) integer, a valid EPSG value
#'   (numeric), or (iii) an object of class `crs`, see [sf::st_crs()].
#'
#' @details
#' \code{changeproj()} transform the columns  "x" and "y" of the slot
#' \code{clusters} of \code{object} and convert \code{boundary} using the new
#' map projection defined by \code{proj}.
#'
#' If applicable, the slot \code{rings} will be recalculated.
#'
#' @return Return \code{object} expressed in the projection \code{proj}.
#' @seealso [sf::st_transform()], [prevR-class]
#' @importFrom sf st_transform st_coordinates st_as_sf
#'
#' @examples
#' print(fdhs)
#' plot(fdhs, axes = TRUE, main = "Projection: longitude/latitude")
#'
#' fdhs2 <- changeproj(
#'   fdhs,
#'   "+proj=utm +zone=30 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
#' )
#' print(fdhs2)
#' plot(fdhs2, axes = TRUE, main = "Projection: UTM Zone 30")
#'
#' @keywords manip spatial
#' @aliases changeproj changeproj-methods
setMethod(
  "changeproj", "prevR",
  function(object, proj) {
    if (!inherits(proj, "crs")) {
      proj <- sf::st_crs(proj)
    }

    # cluster slot modification
    clusters <- slot(object, "clusters")
    clusters_sf <- sf::st_as_sf(clusters, coords = c("x", "y"))
    sf::st_crs(clusters_sf) <- object@proj
    clusters_sf <- sf::st_transform(clusters_sf, proj)
    coord <- sf::st_coordinates(clusters_sf)
    clusters$x <- coord[, 1]
    clusters$y <- coord[, 2]
    slot(object, "clusters") <- clusters

    # boundary slot modification
    boundary <- slot(object, "boundary")
    is.valid <- attr(boundary, "valid")
    boundary <- sf::st_transform(boundary, proj)
    attr(boundary, "valid") <- is.valid
    slot(object, "boundary") <- boundary

    slot(object, "proj") <- proj

    N <- NULL
    R <- NULL
    # On verifie si rings est vide
    if (is.prevR(object, "rings")) {
      rings <- slot(object, "rings")
      N <- sapply(rings, function(x) x$N)
      R <- sapply(rings, function(x) x$R)
    }

    slot(object, "rings") <- list()

    # On recalcule, le cas echeant le slot rings
    if (!is.null(N) && !is.null(R)) {
      object <- rings(object, N = N, R = R)
    }

    object
  }
)
