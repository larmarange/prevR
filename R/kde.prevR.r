#' @exportMethod kde

setGeneric(
  "kde",
  function(
      object,
      N = NULL,
      R = NULL,
      weighted = TRUE,
      risk.ratio = FALSE,
      keep.details = FALSE,
      nb.cells = 100,
      cell.size = NULL,
      progression = TRUE,
      short.names = FALSE) {
    standardGeneric("kde")
  }
)

#' Kernel density estimation for prevR object.
#'
#' This function allows to calculate a prevalence surface (ratio of two
#' intensity surfaces) and/or a relative risks surface (ratio of two density
#' surfaces) using gaussian kernel estimators with adaptative bandwidths of
#' equal number of observations or equal radius.
#'
#' @param object object of class [prevR-class].
#' @param N integer or list of integers corresponding to the rings to use.
#' @param R integer or list of integers corresponding to the rings to use.
#' @param weighted use weighted data (`TRUE`, `FALSE` or `2`)?
#' @param risk.ratio calculate a relative risks surface instead of a
#' prevalence surface (`TRUE`, `FALSE` or `2`)?
#' @param keep.details return surface of positive cases and surface of
#' observed cases?
#' @param nb.cells number of cells on the longest side of the studied area
#'   (unused if \code{cell.size} is defined).
#' @param cell.size size of each cell (in the unit of the projection).
#' @param progression show a progress bar?
#' @param short.names should names of the output be short?
#' @importFrom KernSmooth bkde2D
#'
#' @details This function calculates a prevalence surface as the ratio of the
#' intensity surface (expressed in cases per surface unit) of positive cases
#' on the intensity surface of observed cases and could also calculate a
#' relative risks surface corresponding to the ratio of the density surface
#' (whose integral has been normalized to one) of positive cases on density
#' surface of observed cases.
#'
#' This method is a variant of the nearest neighbor technique. Surfaces are
#' estimated using gaussian kernel estimators with adaptative bandwidths,
#' bandwidth size being determined by a minimum number of
#' observations in the neighborhood (see [rings()] for more details).
#' Fixed bandwidths could also be used. More precisely, the bandwidth used is
#' half the radius of rings of equal number of observations or equal radius
#' (parameters `N` and `R`) calculated by the' function [rings()].
#'
#' See references for a detailed explanation of the implemented methodology.
#'
#' `N` and `R` determine the rings to use for the estimation. If they are not
#' defined, surfaces will be estimated for each available couples (N,R)
#' available in `object`. Several estimations could be
#' simultaneously calculated if several values of N and R are defined.
#'
#' A suggested value of N could be computed with [Noptim()].
#'
#' @return Object of class [sf::sf].
#' Surfaces are named according to the name of the corresponding N and R
#' (for example: \emph{k.prev.N300.RInf}). If `short.names` is `TRUE` and
#' if there is only one combination of couples (N, R), variable names will not
#' be suffixed by the value of N and R.
#'
#' Estimated variables are (depending on the function parameters) :\itemize{
#'     \item "k.pos" unweighted intensity surface of positive cases.
#'     \item "k.obs" unweighted intensity surface of observed cases.
#'     \item "k.prev" unweighted surface of prevalence (k.pos/k.obs).
#'     \item "k.case" unweighted density surface of positive cases.
#'     \item "k.control" unweighted density surface of observed cases.
#'     \item "k.rr" unweighted surface of relative risks (k.case/k.control).
#'     \item "k.wpos" weighted intensity surface of positive cases.
#'     \item "k.wobs" weighted intensity surface of observed cases.
#'     \item "k.wprev" weighted surface of prevalence (k.wpos/k.wobs).
#'     \item "k.wcase" weighted density surface of positive cases.
#'     \item "k.wcontrol" weighted density surface of observed cases.
#'     \item "k.wrr" weighted surface of relative risks (k.wcase/k.wcontrol).
#' }
#'
#' @references Larmarange Joseph, Vallo Roselyne, Yaro Seydou, Msellati Philippe
#' and Meda Nicolas (2011) "Methods for mapping regional trends of HIV
#' prevalence from Demographic and Health Surveys (DHS)",
#' \emph{Cybergeo: European Journal of Geography}, no 558,
#' \url{https://journals.openedition.org/cybergeo/24606},
#' DOI: 10.4000/cybergeo.24606.
#'
#' @note Results could be plotted with [sf::plot()] or with \pkg{ggplot2}
#' using [ggplot2::geom_sf()]. See examples.
#'
#' \pkg{prevR} provides several continuous color palettes
#' (see [prevR.colors]).
#'
#' Results could be turned into a \pkg{stars} raster using
#' [stars::st_rasterize()].
#'
#' To export to ASCII grid, rasterize the results with [stars::st_rasterize()],
#' convert to `SpatRast` with [terra::rast()], extract the desired layer with
#' `[[]]` and then use `terra::writeRaster()`. See examples.
#'
#' See the package \pkg{sparr} for another methodology to estimate relative
#' risks surfaces, adapted for other kind of data than Demographic and
#' Health Surveys (DHS).
#'
#' @seealso [KernSmooth::bkde2D()], [rings()], [Noptim()].
#'
#' @examples
#' \dontrun{
#' dhs <- rings(fdhs, N = c(100, 200, 300, 400, 500))
#'
#' prev.N300 <- kde(dhs, N = 300, nb.cells = 200)
#'
#' plot(prev.N300, lty = 0)
#'
#' library(ggplot2)
#' ggplot(prev.N300) +
#'   aes(fill = k.wprev.N300.RInf) +
#'   geom_sf(colour = "transparent") +
#'   scale_fill_gradientn(colors = prevR.colors.red()) +
#'   theme_prevR_light()
#'
#' # Export k.wprev.N300.RInf surface in ASCII Grid
#' r <- terra::rast(stars::st_rasterize(prev.N300))
#' # writeRaster(r[[2]], "kprev.N300.asc")
#' }
#'
#' @keywords smooth spatial
#' @aliases kde-methods kde,prevR-method kde
#' @importFrom sf st_coordinates

setMethod(
  "kde", "prevR",
  function(
      object,
      N = NULL,
      R = NULL,
      weighted = TRUE,
      risk.ratio = FALSE,
      keep.details = FALSE,
      nb.cells = 100,
      cell.size = NULL,
      progression = TRUE,
      short.names = FALSE) {
    if (is.null(N) && !is.null(R)) N <- Inf
    if (is.null(R) && !is.null(N)) R <- Inf

    # If both are null, we take all combinaisons from the slot rings
    if (is.null(R) && is.null(R)) {
      if (!is.prevR(object, "rings")) {
        stop("the slot 'rings' is empty: you have to run the rings function first.", call. = FALSE) # nolint
      }
      rings <- slot(object, "rings")
      N <- sapply(rings, function(x) x$N)
      R <- sapply(rings, function(x) x$R)
    }

    # If only R, no need of rings
    if (!is.prevR(object, "rings") && any(N != Inf)) {
      stop("the slot 'rings' is empty: you have to run the rings function first.", call. = FALSE) # nolint
    }

    clusters <- slot(object, "clusters")
    ind <- match(c("wpos", "wn"), names(clusters), nomatch = 0)
    if ((weighted == TRUE || weighted == 2) && any(ind == 0)) {
      warning("'wpos' and 'wn' are not present: the 'weighted' argument has been put at FALSE.") # nolint
      weighted <- FALSE
    }
    rings <- slot(object, "rings")

    .isInputOk.prevR(N = N, R = R)
    couples <- unique(data.frame(N = N, R = R, stringsAsFactors = FALSE))

    # Est-on en longitude/latitude ?
    boundary <- slot(object, "boundary")
    proj <- slot(object, "proj")
    longlat <- FALSE
    if (regexpr("longlat", proj$input) != -1 ||
      regexpr("latlong", proj$input) != -1) {
      longlat <- TRUE
    }

    # Calcul de la grille
    grid <- make.grid.prevR(
      object,
      nb.cells = nb.cells,
      cell.size = cell.size
    )
    coord <- sf::st_coordinates(grid)
    range.x <- range(unique(coord[, 1]))
    range.y <- range(unique(coord[, 2]))
    gridsize.x <- length(unique(coord[, 1]))
    gridsize.y <- length(unique(coord[, 2]))

    one.var <- "r.prev"
    result <- NULL

    # Barre de progression
    if (progression) {
      message("Progress of calculations:", domain = "R-prevR")
      barre <- txtProgressBar(
        min = 0,
        max = nrow(couples) * nrow(clusters) + 25,
        initial = 0,
        style = 3
      )
      # On ajoute 25 au max pour prendre en compte le temps de transformation
      # a la fin de la fonction
    }

    for (ic in seq_len(nrow(couples))) {
      one.N <- couples[ic, "N"]
      one.R <- couples[ic, "R"]
      if (any(N != Inf)) {
        ringName <- paste0("N", one.N, ".R", one.R)
        ring <- rings[[ringName]]
        if (is.null(ring)) {
          warning(
            gettextf(
              "no data available for the variable '%s' with N=%s and R=%s.",
              one.var,
              one.N,
              one.R,
              domain = "R-prevR"
            )
          )
          next
        }
        dataCase <- merge(clusters, ring[["estimates"]], by = "id")
      } else { # If only R, we can take directly the value of R
        dataCase <- clusters
        dataCase$r.radius <- one.R
      }
      if (nrow(dataCase) == 0) next

      bw <- dataCase[["r.radius"]]
      bwx <- bw
      bwy <- bw
      x <- dataCase[["x"]]
      y <- dataCase[["y"]]

      # Attention si x et y sont en degres il faut calculer la largeur de bande
      # en kilomtres, 6378.388 correspond au rayon terrestre moyen
      if (longlat) {
        bwx <- bw / (6378.388 * cos(pi * y / 180))
        bwx <- bwx * 180 / pi
        bwy <- bw / 6378.388
        bwy <- bwy * 180 / pi
      }
      k.pos <- 0
      k.obs <- 0
      k.wpos <- 0
      k.wobs <- 0
      for (i in seq_len(length(bw))) {
        temp <- suppressWarnings(
          KernSmooth::bkde2D(
            x = matrix(c(x[i], y[i]), ncol = 2),
            bandwidth = c(bwx[i] / 2, bwy[i] / 2),
            gridsize = c(gridsize.x, gridsize.y),
            range.x = list(range.x, range.y),
            truncate = FALSE
          )
        )
        k.pos <- k.pos + temp$fhat * dataCase[["pos"]][i]
        k.obs <- k.obs + temp$fhat * dataCase[["n"]][i]
        if (weighted == TRUE || weighted == 2) {
          k.wpos <- k.wpos + temp$fhat * dataCase[["wpos"]][i]
          k.wobs <- k.wobs + temp$fhat * dataCase[["wn"]][i]
        }
        if (progression) {
          barre.cur <- (ic - 1) * nrow(clusters) + i
          setTxtProgressBar(barre, value = barre.cur)
        }
      }
      k.case <- k.pos / sum(clusters[["pos"]])
      k.control <- k.obs / sum(clusters[["n"]])
      k.prev <- 100 * k.pos / k.obs
      k.rr <- 100 * k.case / k.control
      if (weighted == TRUE || weighted == 2) {
        k.wcase <- k.wpos / sum(clusters[["wpos"]])
        k.wcontrol <- k.wobs / sum(clusters[["wn"]])
        k.wprev <- 100 * k.wpos / k.wobs
        k.wrr <- 100 * k.wcase / k.wcontrol
      }

      result.one <- NULL

      if (risk.ratio == FALSE || risk.ratio == 2) {
        result.one <- c(
          result.one,
          list(k.pos = k.pos, k.obs = k.obs, k.prev = k.prev)
        )
        if (weighted == TRUE || weighted == 2) {
          result.one <- c(
            result.one,
            list(k.wpos = k.wpos, k.wobs = k.wobs, k.wprev = k.wprev)
          )
        }
      }
      if (risk.ratio == TRUE || risk.ratio == 2) {
        result.one <- c(
          result.one,
          list(k.case = k.case, k.control = k.control, k.rr = k.rr)
        )
        if (weighted == TRUE || weighted == 2) {
          result.one <- c(
            result.one,
            list(k.wcase = k.wcase, k.wcontrol = k.wcontrol, k.wrr = k.wrr)
          )
        }
      }

      varNames <- names(result.one)
      if (!keep.details) {
        ind <- match(
          c("k.prev", "k.wprev", "k.rr", "k.wrr"),
          varNames,
          nomatch = 0
        )
        result.one <- result.one[ind]
      }

      if (!short.names | nrow(couples) > 1) {
        names(result.one) <- paste0(names(result.one), ".N", one.N, ".R", one.R)
      }

      if (is.null(result)) {
        result <- result.one
      } else {
        result <- c(result, result.one)
      }
    }

    # Ajout des coordonnees
    result <- c(list(x = temp$x1, y = temp$x2), result)

    # Passage en data.frame
    result <- xyz2dataframe(result, "x", "y", names(result)[c(-1, -2)])
    if (progression) {
      barre.cur <- nrow(couples) * nrow(clusters) + 15
      setTxtProgressBar(barre, value = barre.cur)
    }
    # puis en sf
    result <- sf::st_as_sf(result, coords = c("x", "y"))
    sf::st_crs(result) <- object@proj
    # transform to polygons
    result <- stars::st_rasterize(result)
    result <- sf::st_as_sf(result)

    boundary <- slot(object, "boundary")
    if (attr(boundary, "valid")) {
      result <- st_filter_prevR(result, boundary)
    }

    if (progression) {
      barre.cur <- nrow(couples) * nrow(clusters) + 25
      setTxtProgressBar(barre, value = barre.cur)
      close(barre)
    }

    result
  }
)
