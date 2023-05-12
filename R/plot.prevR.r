#' @exportMethod plot
#' @importFrom sp plot

if (!isGeneric("plot")) {
  setGeneric("plot", function(x, y, ...) standardGeneric("plot"))
}

#' Plot object of class prevR.
#'
#' Method `plot` for object of class [prevR-class].
#' Plot clusters, number of observations per cluster or number of positive cases
#' per cluster.
#'
#' @param x object of class [prevR-class].
#' @param type graph to plot:\itemize{
#'   \item "position" clusters position.
#'   \item "c.type" clusters per \code{c.type}.
#'   \item "count" number of observations per cluster.
#'   \item "flower" number of positive cases per cluster.
#' }
#' @param add.legend add a legend?
#' @param legend.location legend location.
#' @param factor.size scale factor of rings (for \code{type="count"}).
#' @param new.window plot in a new window?
#' @param axes show axes?
#' @param ... additional arguments transmitted to [graphics::title()].
#' @import sp
#'
#' @details
#' Available values for `legend.location` are: \emph{"bottomright"},
#' \emph{"bottom"}, \emph{"bottomleft"}, \emph{"left"}, \emph{"topleft"},
#' \emph{"top"}, \emph{"topright"}, \emph{"right"} use \emph{"center"}.
#'
#' Use \code{main} to define a title and \code{sub} for a subtitle
#' (see [graphics::title()]).
#'
#' @seealso [graphics::title()], [graphics::legend()].
#'
#' @examples
#' \dontshow{
#' par(ask = TRUE)
#' }
#' plot(fdhs, type = "position", main = "position", axes = TRUE)
#' plot(fdhs, type = "c.type", main = "c.type")
#' plot(fdhs, type = "count", main = "count", factor.size = 0.1)
#' plot(fdhs, type = "flower", main = "flower")
#' \dontshow{
#' par(ask = FALSE)
#' }
#'
#' @aliases plot plot-methods plot,prevR-method plot,prevR,missing-method
#' @keywords hplot

setMethod(
  "plot", c("prevR", "missing"),
  function(x, type = "position", add.legend = TRUE,
           legend.location = "bottomright", factor.size = 0.2,
           new.window = FALSE, axes = FALSE, ...) {
    if (!is.element(type, c("position", "c.type", "count", "flower"))) {
      stop("the argument 'type' must be 'position', 'count', 'flower' or 'c.type'.", call. = FALSE) # nolint
    }
    clusters <- slot(x, "clusters")
    boundary <- slot(x, "boundary")
    if (new.window) dev.new()
    lty <- 1
    if (!attr(boundary, "valid")) lty <- 0
    plot(boundary, asp = 1, axes = axes, xlab = NA, ylab = NA, lty = lty)

    if (type == "position") {
      points(x = clusters[["x"]], y = clusters[["y"]], pch = 21, bg = "green")
      title(...)
    }

    if (type == "c.type") {
      # Position des clusters par type de residence
      if (is.element("c.type", names(clusters))) {
        c.type <- clusters[["c.type"]]
        if (!is.factor(c.type)) c.type <- factor(c.type)
        niveaux <- levels(c.type)
        pchDep <- 25 - length(niveaux)
        points(
          x = clusters[["x"]],
          y = clusters[["y"]],
          pch = pchDep + as.numeric(c.type),
          col = "black",
          bg = 1 + as.numeric(c.type)
        )
        if (add.legend) {
          legend(
            legend.location,
            legend = niveaux,
            pch = pchDep + seq_along(length(niveaux)),
            col = "black",
            pt.bg = 1 + seq_along(length(niveaux))
          )
        }
        title(...)
      }
    }
    if (type == "flower") {
      n.positif <- clusters[["pos"]]
      n.positif <- round(n.positif)
      aucun <- clusters[n.positif == 0, ]
      points(
        x = aucun[["x"]],
        y = aucun[["y"]],
        pch = 22,
        bg = "green3",
        cex = 0.7
      )
      sunflowerplot(
        x = clusters[["x"]],
        y = clusters[["y"]],
        number = n.positif,
        add = TRUE,
        seg.lwd = 1.5,
        seg.col = "red3",
        cex = 0.8,
        bg = "VioletRed4",
        pch = 21
      )
      title(...)
      if (add.legend) {
        legend(legend.location,
          legend = gettext(
            c(
              "No positive case",
              "Only one positive case",
              "One positive case by 'petal'"
            ),
            domain = "R-prevR"
          ),
          pch = c(22, 21, 8),
          pt.bg = c("green3", "VioletRed4", "red3"),
          col = c("black", "black", "red3")
        )
      }
    }
    if (type == "count") {
      points(
        x = clusters[["x"]],
        y = clusters[["y"]],
        pch = 21,
        bg = "skyblue3",
        cex = clusters[["n"]] * factor.size
      )
      title(...)
      if (add.legend) {
        legend(
          legend.location,
          legend = c("5       ", "10       ", "25"),
          pch = c(21, 21, 21),
          pt.bg = c("skyblue3", "skyblue3", "skyblue3"),
          pt.cex = c(5 * factor.size, 10 * factor.size, 25 * factor.size),
          horiz = TRUE
        )
      }
    }
  }
)
