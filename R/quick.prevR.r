#' Quick prevR analysis and plot
#'
#' This function performs several analysis in one go:
#' (i) apply [rings()];
#' (ii) compute prevalence surface with [kde()];
#' (iii) compute the surface of rings radii with [krige()];
#' (iv) plot prevalence surface using [prevR.colors.red()] and add rings radii
#' as a contour plot.
#'
#' @param object object of class [prevR-class].
#' @param N integer or list of integers corresponding to the rings to use.
#' @param nb.cells number of cells on the longest side of the studied area
#'   (unused if \code{cell.size} is defined).
#' @param cell.size size of each cell (in the unit of the projection).
#' @param weighted use weighted data (TRUE, FALSE or "2")?
#' @param plot.results plot the results?
#' @param return.results return the results?
#' @param return.plot return the plot within the results?
#' @param legend.title title of the legend
#' @param cex to control the text size on the graph
#' @param progression show a progress bar?
#'
#' @import ggplot2
#'
#' @details
#' \code{N} determine the rings to use for the estimation.
#' By default, a suggested value of N will be computed with [Noptim()].
#'
#' @return A list of one or several elements, depending on the arguments:
#' (i) \code{prev} is a \code{SpatialPixelsDataFrame} containing the prevalence
#' surface; (ii) \code{radius} a \code{SpatialPixelsDataFrame} containing the
#' kriged surface of the rings radii; (iii) \code{plot} a \code{ggplot} graph.
#'
#' @seealso [Noptim()], [rings()], [kde()] and [krige()].
#'
#' @examples
#' \dontrun{
#' quick.prevR(fdhs)
#' }
#'
#' @export
#' @keywords spatial smooth plot

quick.prevR <- function(
    object,
    N = Noptim(object),
    nb.cells = 100,
    cell.size = NULL,
    weighted = NULL,
    plot.results = TRUE,
    return.results = FALSE,
    return.plot = FALSE,
    legend.title = "%",
    cex = 0.7,
    progression = TRUE
  ) {
  if (!is.prevR(object)) {
    stop("'object' must be of class prevR.")
  }
  if (is.null(weighted)) { # If not precised, weighted data if available
    weighted <- "wn" %in% colnames(object@clusters)
  }
  if (weighted && !("wn" %in% colnames(object@clusters))) {
    stop("No weighted data found in object.")
  }

  object <- rings(object, N = N, R = Inf, progression = progression)
  prev <- kde(
    object,
    N = N,
    R = Inf,
    weighted = weighted,
    nb.cells = nb.cells,
    cell.size = cell.size,
    progression = progression
  )
  radius <- krige(
    r.radius ~ 1,
    object,
    N = N,
    R = Inf,
    nb.cells = nb.cells,
    cell.size = cell.size,
    debug.level = if (progression) -1 else 0
  )

  if ((plot.results || return.plot)) {
    prev.df <- as.data.frame(stars::st_rasterize(prev))
    radius.df <- as.data.frame(stars::st_rasterize(radius))

    prev.df <- na.omit(prev.df)
    radius.df <- na.omit(radius.df)

    prev.df <- prev.df[order(prev.df$x, prev.df$y), ]
    radius.df <- radius.df[order(radius.df$x, radius.df$y), ]

    r <- data.frame()
    for (n in N) {
      if (weighted) {
        p.name <- paste0("k.wprev.N", n, ".RInf")
      } else {
        p.name <- paste0("k.prev.N", n, ".RInf")
      }
      r.name <- paste0("r.radius.N", n, ".RInf")
      temp <- data.frame(
        N = n,
        x = prev.df[["x"]],
        y = prev.df[["y"]],
        prev = prev.df[[p.name]],
        radius = radius.df[[r.name]]
      )
      r <- rbind(r, temp)
    }

    r$N <- factor(r$N, levels = N, labels = paste0("N=", N))

    p <-
      ggplot2::ggplot(
        r,
        ggplot2::aes(
          x = .data[["x"]],
          y = .data[["y"]],
          fill = .data[["prev"]],
          z = .data[["radius"]]
        )
      ) +
      ggplot2::geom_raster() +
      ggplot2::scale_fill_gradientn(colours = prevR.colors.red(20)) +
      ggplot2::stat_contour(
        ggplot2::aes(colour = ggplot2::after_stat(level), fill = NULL)
      ) +
      ggplot2::scale_colour_continuous(low = "grey50", high = "grey50") +
      ggplot2::facet_wrap(~N) +
      ggplot2::coord_fixed() +
      ggplot2::xlab("") +
      ggplot2::ylab("") +
      ggplot2::labs(fill = legend.title) +
      theme_prevR_light()

    p <- direct.label_prevR(p, list("top.pieces", cex = cex))
  }

  if (plot.results) print(p)

  if (return.results && !return.plot) {
    return(list(prev = prev, radius = radius, tidy = r))
  } else if (return.results && return.plot) {
    return(list(prev = prev, radius = radius, tidy = r, plot = p))
  } else if (!return.results && return.plot) {
    return(list(tidy = r, plot = p))
  }
}

# to avoid no visible binding in after_stat(level)
utils::globalVariables("level")
