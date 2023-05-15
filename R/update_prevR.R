#' Update a prevR object
#'
#' Update an object of class `prevR` created with a previous version of the
#' package to the last version. In particular, it will convert any boundary
#' slot defined with the `sp` package to `sf` class.
#'
#' @param object a `prevR` object
#' @return a `prevR` object
#' @export
update_prevR <- function(object) {
  if (!inherits(object, "prevR"))
    stop("'object' should be of class 'prevR'.")

  if (attr(object@boundary, "valid"))
    boundary <- as(object@boundary, "sf")
  else
    boundary <- NULL
  col <- colnames(object@clusters)
  names(col) <- col

  res <- as.prevR(
    data = object@clusters,
    col = col,
    boundary = boundary,
    proj = st_crs(object@proj)
  )

  N <- NULL
  R <- NULL
  # On verifie si rings est vide
  if (is.prevR(object, "rings")) {
    rings <- slot(object, "rings")
    N <- sapply(rings, function(x) x$N)
    R <- sapply(rings, function(x) x$R)
  }
  # On recalcule, le cas echeant le slot rings
  if (!is.null(N) && !is.null(R)) {
    res <- rings(res, N = N, R = R)
  }

  res
}
