#' Spatial interpolation (kriging and inverse distance weighting) for objects
#' of class prevR.
#'
#' These functions execute a spatial interpolation of a variable of the slot
#' \code{rings} of an object of class [prevR-class]. The method \code{krige()}
#' implements the ordinary kriging technique. The method \code{idw()} executes
#' an inverse distance weighting interpolation.
#'
#' @param formula variable(s) to interpolate (see details).
#' @param locations object of class [prevR-class].
#' @param N integer or list of integers corresponding to the rings to use.
#' @param R integer or list of integers corresponding to the rings to use.
#' @param model a variogram model returned by the function [gstat::vgm()].
#' @param nb.cells number of cells on the longest side of the studied area
#'   (unused if \code{cell.size} is defined).
#' @param cell.size size of each cell (in the unit of the projection).
#' @param fit `"auto"` for using a variogram automatically fitted from the data,
#'   only if \code{model} is not defined (`NULL`).
#' @param keep.variance return variance of estimates?
#' @param show.variogram plot the variogram?
#' @param  idp inverse distance weighting power (see [gstat::idw()]).
#' @param ... additional arguments transmitted to [gstat::krige()]
#'   or [gstat::idw()].
#'
#' @importMethodsFrom gstat krige
#' @importFrom gstat variogram vgm fit.variogram
#' @importFrom sf st_drop_geometry
#' @importFrom stars st_rasterize
#'
#' @details `formula` specifies the variable(s) to interpolate. Only variables
#' available in the slot `rings` of `locations` could be used. Possible values
#' are `"r.pos"`, `"r.n"`, `"r.prev"`, `"r.radius"`, `"r.clusters"`, `"r.wpos"`,
#' `"r.wn"` or `"r.wprev"`. Variables could be specified with a character
#' string or a formula (example: `list(r.pos ~ 1, r.prev ~ 1}`. Only formula
#' like `variable.name ~ 1` are accepted. For more complex interpolations,
#' use directly functions [gstat::krige()] and [gstat::idw()] from \pkg{gstat}.
#'
#' `N` and `R` determine the rings to use for the interpolation. If they are
#' not defined, surfaces will be estimated for each available couples (N,R).
#' Several interpolations could be simultaneously calculated if several
#' variables and/or several values of N and R are defined.
#'
#' A suggested value of N could be computed with [Noptim()].
#'
#' In the case of an ordinary kriging, the method [krige()] from \pkg{prevR}
#' will try to fit automatically a exponential variogram to the sample variogram
#' (`fit = "auto"`). You can also specify directly the variogram to use with
#' the parameter `model`.
#'
#' Interpolations are calculated on a spatial grid obtained with
#' [make.grid.prevR()].
#'
#' @return Object of class [sf::sf].
#' The name of estimated surfaces depends on the name of the interpolated
#' variable, N and R (for example: \emph{r.radius.N300.RInf}).
#' If you ask the function to return variance (`keep.variance=TRUE`),
#' corresponding surfaces names will have the suffix \emph{.var}.
#'
#' @references
#' Larmarange Joseph, Vallo Roselyne, Yaro Seydou, Msellati Philippe and Meda
#' Nicolas (2011) "Methods for mapping regional trends of HIV prevalence from
#' Demographic and Health Surveys (DHS)",
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
#' @seealso [gstat::krige()], [gstat::idw()], [rings()], [Noptim()].
#'
#' @examples
#'   \dontrun{
#'     dhs <- rings(fdhs, N = c(100,200,300,400,500))
#'     radius.N300 <- krige('r.radius', dhs, N = 300, nb.cells = 50)
#'     prev.krige <- krige(r.wprev ~ 1, dhs, N = c(100, 300, 500))
#'
#'     plot(prev.krige, lty = 0)
#'
#'     library(ggplot2)
#'     ggplot(prev.krige) +
#'       aes(fill = r.wprev.N300.RInf) +
#'       geom_sf(colour = "transparent") +
#'       scale_fill_gradientn(colors = prevR.colors.red()) +
#'       theme_prevR_light()
#'
#'     # Export r.wprev.N300.RInf surface in ASCII Grid
#'     r <- terra::rast(stars::st_rasterize(prev.krige))
#'     # writeRaster(r[[2]], "wprev.N300.asc")
#'   }
#'
#' @keywords smooth spatial
#' @exportMethod krige
#' @aliases krige,prevR-method krige-methods krige,ANY,prevR-method krige
setMethod(
  "krige", c(formula = "ANY", locations = "prevR"),
  function(formula, locations, N = NULL, R = Inf, model = NULL,
           nb.cells = 100, cell.size = NULL, fit = "auto",
           keep.variance = FALSE, show.variogram = FALSE, ...) {
    object <- locations
    if (!is.prevR(object, "rings")) {
      stop(
        "the slot 'rings' is empty: you have to run the rings function first.",
        call. = FALSE
      )
    }

    # On accepte que l'on passe a formula une formule ou une liste de formule
    # Cependant, seuls les formules de la forme variable~1 sont acceptees
    if (inherits(formula, "formula")) {
      formula <- list(formula)
    }
    if (is.list(formula)) {
      for (i in seq_len(length(formula))) {
        formule <- formula[[i]]
        if (inherits(formule, "formula")) {
          if (formule[[3]] != 1 || length(all.names(formule)) != 2) {
            stop(
              gettextf(
                paste0(
                  "%s is not a valid formula: ",
                  "krige.prevR only implement simple or ordinary kriging, ",
                  "so formula must have only one variable and ",
                  "no predictor (like 'var~1')."
                ),
                formule,
                domain = "R-prevR"
              )
            )
          }
          formula[[i]] <- all.vars(formule)
        }
      }
      formula <- as.character(formula)
    }
    if (is.null(model) && !is.element(fit, "auto")) {
      stop("the 'fit' argument must be 'auto'.")
    }
    if (inherits(model, "variogramModel")) model <- list(model)

    clusters <- slot(object, "clusters")
    rings <- slot(object, "rings")
    if (is.null(N)) N <- sapply(rings, function(x) x$N)
    if (is.null(R)) R <- sapply(rings, function(x) x$R)

    if (length(model) != 0) {
      .isInputOk.prevR(formula = formula, N = N, R = R, model = model)
    } else {
      .isInputOk.prevR(formula = formula, N = N, R = R)
    }
    couples <- unique(
      data.frame(
        var = formula,
        N = N,
        R = R,
        stringsAsFactors = FALSE
      )
    )
    if (length(model) == 1) model <- rep(model, nrow(couples))

    locations.data <- make.grid.prevR(
      object,
      nb.cells = nb.cells,
      cell.size = cell.size
    )

    i <- 0
    list.variogram <- list()
    first <- TRUE
    for (ic in seq_len(nrow(couples))) {
      one.var <- couples[ic, "var"]
      one.N <- couples[ic, "N"]
      one.R <- couples[ic, "R"]
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
      formule <- formula(paste(one.var, "~1"))

      dataCase <- merge(
        clusters[, c("id", "x", "y")],
        ring[["estimates"]],
        by = "id"
      )
      dataCase <- sf::st_as_sf(dataCase, coords = c("x", "y"))
      sf::st_crs(dataCase) <- object@proj
      if (nrow(dataCase) == 0) next
      if (length(model) != 0) one.model <- model[[ic]]
      if (is.null(model) && fit == "auto") {
        sample.vario <- gstat::variogram(formule, dataCase)
        param <- .init.exp.model.variogram(
          sample.vario[, "dist"],
          sample.vario[, "gamma"]
        )
        if (is.null(param)) {
          warning(
            gettextf(
              paste0(
                "problem to fit the variogram: the variable '%s' ",
                "width N=%s and R=%s has not been treated."
              ),
              one.var,
              one.N,
              one.R,
              domain = "R-prevR"
            )
          )
          next
        }
        one.model <- try(
          gstat::fit.variogram(
            sample.vario,
            model = gstat::vgm(param[1], "Exp", param[2])
          ),
          silent = TRUE
        )
        if (inherits(one.model, "try-error") || attr(one.model, "singular")) {
          one.model <- gstat::vgm(param[1], "Exp", param[2])
        }
      }

      result.one <- krige(
        formule,
        dataCase,
        locations.data,
        model = one.model,
        ...
      )
      if (length(model) == 0) {
        i <- i + 1
        list.variogram[[i]] <- list(
          parameters = c(one.var, one.N, one.R),
          sample.vario = sample.vario,
          model = one.model
        )
      }
      names(result.one) <- c(
        paste0(one.var, ".N", one.N, ".R", one.R),
        paste0(one.var, ".N", one.N, ".R", one.R, ".var"),
        "geometry"
      )
      if (!keep.variance) {
        result.one <- result.one[, -2]
      }

      if (first) {
        result <- result.one
        first <- FALSE
      } else {
        result <- cbind(
          result,
          sf::st_drop_geometry(result.one)
        )
      }
    }

    if (show.variogram) {
      n1 <- 1
      n2 <- 1
      more <- TRUE
      for (k in seq_len(length(list.variogram))) {
        nn <- ceiling(sqrt(length(list.variogram)))
        ll <- list.variogram[[k]]
        key <- list(
          columns = 3,
          text = list(paste(c(" ", "N :", "R :"), ll$param))
        )
        if (k == length(list.variogram)) more <- FALSE
        print(
          plot(
            ll$sample.vario,
            ll$model,
            key = key
          ),
          split = c(n1, n2, nn, nn),
          more = more
        )
        n1 <- n1 + 1
        if (n1 == nn + 1) {
          n1 <- 1
          n2 <- n2 + 1
        }
      }
    }

    # transform to polygons
    result <- stars::st_rasterize(result)
    result <- sf::st_as_sf(result)

    boundary <- slot(object, "boundary")
    if (attr(boundary, "valid")) {
      result <- st_filter_prevR(result, boundary)
    }

    result
  }
)


#' @rdname krige-ANY-prevR-method
#' @aliases idw-methods idw,ANY,prevR-method idw,prevR-method idw
#' @importFrom gstat idw
#' @exportMethod idw

setMethod(
  "idw", c(formula = "ANY", locations = "prevR"),
  function(formula, locations, N = NULL, R = Inf,
           nb.cells = 100, cell.size = NULL, idp = 2, ...) {
    object <- locations
    keep.variance <- FALSE
    if (!is.prevR(object, "rings")) {
      stop(
        "the slot 'rings' is empty: you have to run the rings function first.", # nolint
        call. = FALSE
      )
    }

    if (inherits(formula, "formula")) {
      formula <- list(formula)
    }
    if (is.list(formula)) {
      for (i in seq_len(length(formula))) {
        formule <- formula[[i]]
        if (inherits(formule, "formula")) {
          if (formule[[3]] != 1 || length(all.names(formule)) != 2) {
            stop(
              gettextf(
                paste0(
                  "%s is not a valid formula: ",
                  "idw.prevR only accept formula with only one ",
                  "variable and no predictor like 'var ~ 1'."
                ),
                formule,
                domain = "R-prevR"
              )
            )
          }
          formula[[i]] <- all.vars(formule)
        }
      }
      formula <- as.character(formula)
    }

    clusters <- slot(object, "clusters")
    rings <- slot(object, "rings")
    if (is.null(N)) N <- sapply(rings, function(x) x$N)
    if (is.null(R)) R <- sapply(rings, function(x) x$R)
    .isInputOk.prevR(formula = formula, N = N, R = R)

    couples <- unique(
      data.frame(
        var = formula,
        N = N,
        R = R,
        stringsAsFactors = FALSE
      )
    )

    locations.data <- make.grid.prevR(
      object,
      nb.cells = nb.cells,
      cell.size = cell.size
    )

    first <- TRUE

    for (ic in seq_len(nrow(couples))) {
      one.var <- couples[ic, "var"]
      one.N <- couples[ic, "N"]
      one.R <- couples[ic, "R"]
      ringName <- paste0("N", one.N, ".R", one.R)
      ring <- rings[[ringName]]
      if (is.null(ring)) {
        warning(
          gettextf(
            "no data available for the variable '%s' with N=%s and R=%s.", # nolint
            one.var,
            one.N,
            one.R,
            domain = "R-prevR"
          )
        )
        next
      }
      formule <- formula(paste(one.var, "~ 1"))

      dataCase <- merge(
        clusters[, c("id", "x", "y")],
        ring[["estimates"]],
        by = "id"
      )
      dataCase <- sf::st_as_sf(dataCase, coords = c("x", "y"))
      sf::st_crs(dataCase) <- object@proj

      if (nrow(dataCase) == 0) next

      result.one <- gstat::idw(
        formule,
        locations = dataCase,
        newdata = locations.data,
        idp = idp,
        ...
      )

      names(result.one) <- c(
        paste0(one.var, ".N", one.N, ".R", one.R),
        paste0(one.var, ".N", one.N, ".R", one.R, ".var"),
        "geometry"
      )
      if (!keep.variance) {
        result.one <- result.one[, -2]
      }

      if (first) {
        result <- result.one
        first <- FALSE
      } else {
        result <- cbind(
          result,
          sf::st_drop_geometry(result.one)
        )
      }
    }

    # transform to polygons
    result <- stars::st_rasterize(result)
    result <- sf::st_as_sf(result)

    boundary <- slot(object, "boundary")
    if (attr(boundary, "valid")) {
      result <- st_filter_prevR(result, boundary)
    }

    result
  }
)
