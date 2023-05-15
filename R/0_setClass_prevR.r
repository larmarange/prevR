#' Objects of class prevR.
#'
#' Class used by the package \pkg{prevR}
#'
#' @section Objects from the Class:
#' Objects of this class could be created by the function [as.prevR()].
#'
#' @slot clusters \code{data.frame} with observed data (one line per cluster).
#' Columns names are:\itemize{
#'   \item "id" cluster ID.
#'   \item "x" longitude.
#'   \item "y" latitude.
#'   \item "n" number of valid observations per cluster.
#'   \item "pos" number of positive cases per cluster.
#'   \item "prev" observed prevalence (in %) in the cluster (pos/n).
#'   \item "wn" (optional) sum of weights of observations per cluster.
#'   \item "wpos" (optional) sum of weights of positive cases per cluster.
#'   \item "wprev" (optional) weighted observed prevalence (in %)
#'     in the cluster (wpos/wn).
#'   \item "c.type" (optional) cluster type.
#' }
#'
#' @slot boundary object of class [sf::sf],
#' borders of the studied area.
#'
#' @slot proj object of class [sf::crs][sf::st_crs()], map projection used.
#'
#' @slot rings list of results returned by [rings()].
#' Each entry is composed of 3 elements: \code{N}, minimum number of
#' observations per ring; \code{R}, maximum radius of rings and
#' \code{estimates}, a data frame with the following
#' variables:\itemize{
#'   \item "id" cluster ID.
#'   \item "r.pos" number of positive cases inside the ring.
#'   \item "r.n" number of valid observations inside the ring.
#'   \item "r.prev" observed prevalence (in \%) inside the ring (r.pos/r.n).
#'   \item "r.radius" ring radius (in kilometers if coordinates in
#'     decimal degrees, in the unit of the projection otherwise).
#'   \item "r.clusters" number of clusters located inside the ring.
#'   \item "r.wpos" (optional) sum of weights of positive cases inside the ring.
#'   \item "r.wn" (optional) sum of weights of valid observations inside the
#'     ring.
#'   \item "r.wprev" (optional) weighted observed prevalence (in %) inside
#'     the ring (r.wpos/r.wn).
#' }
#' Note: the list \code{rings} is named, the name of each element is
#' N\emph{N_value}.R\emph{R_value}, for example \emph{N300.RInf}.
#'
#' @section Methods: \describe{
#'   \item{as.data.frame}{\code{signature(x = "prevR")} converts an object of
#'     class prevR into a data frame.}
#'   \item{as.SpatialGrid}{\code{signature(object = "prevR")} generates a
#'     spatial grid.}
#'   \item{export}{\code{signature(object = "prevR")} exports a prevR object as
#'     a shapefile, a dbase file or a text file.}
#'   \item{idw}{\code{signature(formula = "ANY", locations = "prevR")}
#'     calculates a spatial interpolation using an inverse distance weighting.}
#'   \item{kde}{\code{signature(object = "prevR")} estimates a prevalence
#'   surface using kernel density estimators.}
#'   \item{krige}{\code{signature(formula = "ANY", locations = "prevR")}
#'     calculates a spatial interpolation by kriging.}
#'   \item{plot}{\code{signature(x = "prevR", y = "ANY")} plots data of a
#'     prevR object.}
#'   \item{print}{\code{signature(x = "prevR")} shows a summary of a prevR
#'     object.}
#'   \item{rings}{\code{signature(object = "prevR")} calculates rings of equal
#'     number of observations and/or equal radius.}
#'   \item{show}{\code{signature(object = "prevR")} shows a summary of a prevR
#'     object.}
#'   \item{summary}{\code{signature(object = "prevR")} shows a summary of the
#'     variables of a prevR object.}
#'   \item{changeproj}{\code{signature(object = "prevR")} changes the map
#'     projection used.}
#' }
#'
#' @seealso
#' [as.prevR()], [is.prevR()], [changeproj()], [rings()], [print()], [plot()],
#' [summary()], [kde()], [krige()], [idw()], [export()].
#'
#' @examples
#' showClass("prevR")
#'
#' col <- c(
#'   id = "cluster",
#'   x = "x",
#'   y = "y",
#'   n = "n",
#'   pos = "pos",
#'   c.type = "residence",
#'   wn = "weighted.n",
#'   wpos = "weighted.pos"
#' )
#' dhs <- as.prevR(fdhs.clusters, col, fdhs.boundary)
#' str(dhs)
#' print(dhs)
#'
#' \dontrun{
#' dhs <- rings(fdhs, N = c(100, 300, 500))
#' str(dhs)
#' print(dhs)
#' }
#' @keywords classes
#' @export
#' @importFrom sf st_crs

methods::setClass(
  Class = "prevR",
  representation(
    clusters = "data.frame",
    boundary = "sf",
    proj = "crs",
    rings = "list"
  ),
  validity = function(object) {
    clusters <- slot(object, "clusters")
    necessaryVar <- c("id", "x", "y", "n", "pos")
    ind <- match(necessaryVar, names(clusters))
    if (any(is.na(ind))) {
      missing.var <- paste(necessaryVar[is.na(ind)], collapse = ", ")
      n.missing <- length(necessaryVar[is.na(ind)])
      stop.mess <- sprintf(
        ngettext(
          n.missing,
          "the variable %s is missing.",
          "the following variables (%s) are missing.",
          domain = "R-prevR"
        ),
        missing.var
      )
      stop(stop.mess, call. = FALSE)
    }

    coupledVar <- c("wn", "wpos")
    ind <- match(coupledVar, names(clusters))
    ind <- ind[!is.na(ind)]
    if (length(ind) == 1) {
      stop(
        gettextf(
          "the wn and wpos variables are coupled and you have only defined %s.",
          names(clusters)[ind],
          domain = "R-prevR"
        ),
        call. = FALSE
      )
    }
    proj <- slot(object, "proj")
    isOk <- try(sf::st_crs(proj), silent = TRUE)
    if (inherits(isOk, "try-error")) {
      stop(
        gettextf(
          "the projection %s, defined in the 'proj' argument, is incorect.",
          proj,
          domain = "R-prevR"
        ),
        call. = FALSE
      )
    }

    boundary <- slot(object, "boundary")
    boundary.proj <- sf::st_crs(boundary)
    if (format(boundary.proj) != format(proj)) {
      stop(
        "'boundary' and 'clusters' didn't have the same projection.",
        call. = FALSE
      )
    }
    TRUE
  }
)
