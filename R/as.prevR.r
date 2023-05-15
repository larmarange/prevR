#' Create an object of class prevR.
#'
#' This function creates an object of class [`prevR-class`] from a data frame.
#'
#' @param data data frame, each line corresponding to an observed cluster.
#' @param col vector identifying the columns of \code{data} to use.\cr
#'  \code{clusters} columns names are fixed:\itemize{
#'    \item "id" (optional) cluster's identifier.
#'    \item "x" cluster's longitude.
#'    \item "y" cluster's latitude.
#'    \item "n" number of valid observations in the cluster.
#'    \item "pos" number of positive cases in the cluster.
#'    \item "wn" (optional) sum of observations weight.
#'    \item "wpos" (optional) sum of positive cases weight.
#'    \item "c.type" (optional) type of cluster (used only by [plot()]).
#'  }
#'  See examples.
#' @param boundary object of class [sf::sf] defining the studied area.
#' @param proj projection of clusters coordinates used in \code{data}
#'   (longitude and latitude in decimal degrees by default). One of
#'   (i) character: a string accepted by GDAL, (ii) integer, a valid EPSG value
#'   (numeric), or (iii) an object of class `crs`, see [sf::st_crs()].
#' @details
#' Only "x", "y" "n" and "pos" are required in `col`.
#' If "id" is not specified, a numerical identifier will be automatically
#' created.
#'
#' If `boundary` is not defined (`NULL`), a rectangle corresponding to minimal
#' and maximal coordinates of `data` will be used.
#'
#' `boundary` could be the result of the function [create.boundary()].
#'
#' It's not possible to change projection of `data` with `as.prevR()`.
#' Use [changeproj()] instead.
#'
#' @return Object of class [`prevR-class`]
#'
#' @seealso [`prevR-class`] class, [create.boundary()],
#' [changeproj()], [import.dhs()].
#'
#' @examples
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
#'
#' str(dhs)
#' print(dhs)
#' @keywords manip
#' @export
#' @importFrom sf st_polygon st_sfc st_sf st_transform
as.prevR <- function(data,
                     col,
                     boundary = NULL,
                     proj = "+proj=longlat +datum=WGS84") {
  if (!is.data.frame(data)) {
    stop("the 'data' argument must be a dataframe.", call. = FALSE)
  }

  ind <- match(col, names(data))
  if (any(is.na(ind))) {
    missing.var <- paste(col[is.na(ind)], collapse = ", ")
    n.missing <- length(col[is.na(ind)])
    stop.mess <- sprintf(
      ngettext(
        n.missing,
        "the variable %s, defined in 'col', is not present in 'data'.",
        "the following variables (%s), defined in 'col', are not present in 'data'.", # nolint
        domain = "R-prevR"
      ),
      missing.var
    )
    stop(stop.mess, call. = FALSE)
  }

  col <- col[!is.null(col)]
  data <- data[, col]
  names(data) <- names(col)
  necessaryVar <- c("id", "x", "y", "n", "pos")
  ind <- match(necessaryVar, names(data))
  if (any(is.na(ind))) {
    missing.var <- paste(necessaryVar[is.na(ind)], collapse = ", ")
    n.missing <- length(necessaryVar[is.na(ind)])
    stop.mess <- sprintf(
      ngettext(
        n.missing,
        "the variable %s is missing in 'col'.",
        "the following variables (%s) are missing in 'col'.",
        domain = "R-prevR"
      ),
      missing.var
    )
    stop(stop.mess, call. = FALSE)
  }

  if (!is.element("id", names(data))) {
    data$id <- seq_len(nrow(data))
  }

  utilsVar <- c(
    "id", "x", "y", "n", "pos", "prev",
    "wn", "wpos", "wprev", "c.type"
  )
  ind <- match(names(data), utilsVar, nomatch = 0)
  if (any(ind == 0)) {
    cancelled.var <- paste(names(data)[ind == 0], collapse = ", ")
    n.cancelled <- length(names(data)[ind == 0])
    mess <- sprintf(
      ngettext(
        n.cancelled,
        "The variable %s has been cancelled from 'data'.",
        "The following variables (%s) have been cancelled from 'data'.",
        domain = "R-prevR"
      ),
      cancelled.var
    )
    warning(mess)
  }

  # Reduire data apres avoir calcule le message d'information.
  data <- data[, ind != 0]

  # On force c.type a etre du type factor
  if (!is.null(data$c.type) && !is.factor(data$c.type)) {
    data$c.type <- as.factor(data$c.type)
  }

  # On calcule si besoin prev et wprev
  if (is.null(data$prev)) {
    data$prev <- 100 * data$pos / data$n
  }
  if (is.null(data$wprev) && !is.null(data$wpos) && !is.null(data$wn)) {
    data$wprev <- 100 * data$wpos / data$wn
  }

  isOk <- try(sf::st_crs(proj), silent = TRUE)
  if (inherits(isOk, "try-error") || is.na(isOk)) {
    stop(
      gettextf(
        "the projection %s, defined in the 'proj' argument, is incorect.",
        proj,
        domain = "R-prevR"
      ),
      call. = FALSE
    )
  }
  projCRS <- sf::st_crs(proj)
  # Si boundary n'existe pas il faut en creer un fictif pour que le slot
  # boundary de la classe prevR puisse etre renseigne
  # On cree donc un objet de classe sf fictif et on lui donne
  # un attribut "valid" que l'on positionne a FALSE
  if (is.null(boundary)) {
    x <- data[, "x"]
    y <- data[, "y"]
    xx <- c(min(x), min(x), max(x), max(x), min(x))
    yy <- c(min(y), max(y), max(y), min(y), min(y))
    boundary <- sf::st_polygon(list(cbind(xx, yy)))
    boundary <- sf::st_sfc(boundary)
    boundary <- sf::st_sf(boundary)
    sf::st_crs(boundary) <- projCRS
    attr(boundary, "valid") <- FALSE
  } else {
    if (!inherits(boundary, "sf")) {
      stop("the class of 'boundary' must be sf.", call. = FALSE)
    }
    # On teste si boundary contient une projection.
    # Si pas de projection, on suppose que boundary est dans la meme projection
    # que clusters.Sinon, on transforme boundary a la volee pour le passer dans
    # la meme projection que clusters
    if (is.na(sf::st_crs(boundary))) {
      sf::st_crs(boundary) <- projCRS
      message(
        gettextf(
          paste0(
            "No projection was defined in 'boundary' argument: 'boundary' has ",
            "then be considered to be in the same projection (%s) as 'data'."
          ),
          proj,
          domain = "R-prevR"
        )
      )
    } else {
      boundary <- sf::st_transform(boundary, projCRS)
    }
    attr(boundary, "valid") <- TRUE
  }
  new(
    "prevR",
    clusters = data,
    proj = projCRS,
    boundary = boundary,
    rings = list()
  )
}
