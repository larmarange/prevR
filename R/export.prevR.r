#' @exportMethod export
setGeneric(
  "export",
  function(object, element, format, file,
           N = NULL, R = NULL,
           clusters.only = FALSE,
           ext = NULL, sep = NULL, dec = NULL,
           ...) {
    standardGeneric("export")
  }
)

#' Export an object of class prevR.
#'
#' This method could be used to export an object of class [prevR-class]
#' in different formats (text, shapefile, dbase...)
#'
#' @param object object of class [prevR-class].
#' @param element element to export: "clusters" or "boundary".
#' @param format format: "dbf", "txt", csv", "csv2" or "shp"
#'   (unused if \code{element = "boundary"}).
#' @param file file name **without** extension.
#' @param N integer or list of integers setting elements of \code{rings}
#'   to export (unused if \code{element="boundary"}).
#' @param R integer or list of integers setting elements of \code{rings}
#'   to export (unused if \code{element="boundary"}).
#' @param clusters.only export only the slot \code{clusters} of \code{object}
#'   (unused if \code{element="boundary"})?
#' @param ext coerce the extension of the export file
#'   (unused if \code{element="boundary"} or if \code{format="shp"}).
#' @param sep coerce the field separator string
#'   (unused if \code{element="boundary"}
#'   or if \code{format="shp"} or if \code{format="dbf"}).
#' @param dec coerce the string to use for decimal point
#'   (unused if \code{element="boundary"} or if \code{format="shp"}
#'   or if \code{format="dbf"}).
#' @param ... additional arguments transmitted to
#'   [sf::st_write], [foreign::write.dbf()] or
#'   [utils::write.table()].
#' @importFrom foreign write.dbf
#' @details If \code{element="boundary"}, the slot \code{boundary} of
#' \code{object} will be exported as a \emph{shapefile}.
#'
#' Otherwise, the slot \code{clusters}, merged with the slot \code{rings},
#' will be exported.
#'
#' See [as.data.frame()] for details on the use of the parameters of \code{N},
#' \code{R} et \code{clusters.only}.
#'
#' \code{format} specifies the export format of the data frame returned by
#' [as.data.frame()]: \tabular{ll}{
#'     "shp" \tab Shape File\cr
#'     "dbf" \tab DBASE format\cr
#'     "txt" \tab tabulated text\cr
#'     "csv" \tab 'comma separated values'\cr
#'     "csv2" \tab CSV variant using a semicolon as field separator
#' }
#' \code{ext} could be used to coerce the extension of the output file,
#' except for \emph{shapefile} export, which will write four different files
#' (.shp, .shx, .dbf and .prj).
#'
#' The "txt" format uses by default a tabulation as field separator and
#' a point "." for decimal point.
#'
#' The "csv" format uses a comma "," as field separator and
#' a point "." as decimal point.
#'
#' The "csv2" format is a variant using a semicolon ";" as field separator and
#' a colon "," for decimal point,
#' the Excel convention for CSV files in some Western European locales.
#'
#' \code{sep} and \code{dec} could be used to coerce the field separator and
#' the decimal point (together with the "txt" format).
#'
#' @seealso [sf::st_write()],
#' [foreign::write.dbf()], [utils::write.table()].
#'
#' @examples
#' \dontrun{
#' export(fdhs, element = "boundary", file = "area")
#' export(fdhs, element = "clusters", format = "shp", file = "points")
#'
#' dhs <- rings(fdhs, N = c(100, 300, 500))
#' export(dhs, element = "clusters", format = "csv", N = 300, file = "points")
#' }
#'
#' @aliases export-methods export,prevR-method export
#' @keywords manip spatial
#' @importFrom sf st_write
#' @importFrom foreign write.dbf

setMethod(
  "export", "prevR",
  function(object, element, format, file,
           N = NULL, R = NULL, clusters.only = FALSE,
           ext = NULL, sep = NULL, dec = NULL, ...) {

    ind <- match(element, c("clusters", "boundary"), nomatch = 0)
    if (ind == 0) {
      stop(
        "the 'element' argument must be 'clusters' or 'boundary'.",
        call. = FALSE
      )
    }

    if (element == "boundary") {
      boundary <- slot(object, "boundary")
      file <- paste(file, "shp", sep = ".")
      sf::st_write(boundary, file, ...)
    }

    if (element == "clusters") {
      ind <- match(format, c("dbf", "txt", "shp", "csv", "csv2"), nomatch = 0)
      if (ind == 0) {
        stop(
          "the 'format' argument must be 'dbf', 'txt', 'csv', 'csv2' or 'shp'.",
          call. = FALSE
        )
      }
      clusters <- as.data.frame(
        object,
        N = N,
        R = R,
        clusters.only = clusters.only
      )
      if (format == "shp") {
        clusters_sf <- sf::st_as_sf(clusters, coords = c("x", "y"))
        sf::st_crs(clusters_sf) <- object@proj
        file <- paste(file, "shp", sep = ".")
        sf::st_write(clusters_sf, file, ...)
      }
      if (is.null(ext) && format != "csv2") ext <- format
      if (is.null(ext) && format == "csv2") ext <- "csv"
      file <- paste(file, ext, sep = ".")
      if (format == "txt") {
        if (is.null(sep)) sep <- "\t"
        if (is.null(dec)) dec <- "."
        write.table(clusters, file = file,
                    row.names = FALSE, sep = sep, dec = dec, ...)
      }
      if (format == "csv") {
        if (is.null(sep)) sep <- ","
        if (is.null(dec)) dec <- "."
        write.table(clusters, file = file,
                    row.names = FALSE, sep = sep, dec = dec, ...)
      }
      if (format == "csv2") {
        if (is.null(sep)) sep <- ";"
        if (is.null(dec)) dec <- ","
        write.table(clusters, file = file,
                    row.names = FALSE, sep = sep, dec = dec, ...)
      }
      if (format == "dbf") {
        foreign::write.dbf(clusters, file, ...)
      }
    }
  }
)
