#' Summary of a prevR object.
#'
#' Method \code{show} for objects of class [prevR-class]:
#' shows a summary of the object's characteristics.
#'
#' @param object object of class [prevR-class].
#'
#' @note Exactly the same as [print()].
#' @seealso [summary()]
#' @examples
#' fdhs
#' \dontrun{
#' dhs <- rings(fdhs, N = c(100, 300, 500))
#' dhs
#' }
#'
#' @aliases show show-methods show,prevR-method
#' @exportMethod show
#' @importFrom sf st_bbox
setMethod(
  "show", "prevR",
  function(object) {
    clusters <- slot(object, "clusters")
    boundary <- slot(object, "boundary")

    clustersNumber <- nrow(clusters)
    ObservationNumber <- sum(clusters$n)
    PositiveCases <- sum(clusters$pos)
    isWeightedData <- !any(is.na(match(c("wn", "wpos"), names(clusters))))
    nationalPrev <- 100 * sum(clusters$pos, na.rm = TRUE) /
      sum(clusters$n, na.rm = TRUE)
    if (isWeightedData) {
      weightedNationalPrev <- 100 * sum(clusters$wpos, na.rm = TRUE) /
        sum(clusters$wn, na.rm = TRUE)
    }
    proj <- format(object@proj)
    coordinatesRange <- rbind(
      range(clusters$x, na.rm = TRUE),
      range(clusters$y, na.rm = TRUE)
    )
    dimnames(coordinatesRange) <- list(c("x", "y"), c("min", "max"))
    boundaryCoordinatesRange <- NULL
    if (attr(boundary, "valid")) {
      boundaryCoordinatesRange <- sf::st_bbox(boundary)
    }

    message("Object of class 'prevR'\n", domain = "R-prevR")

    message(
      gettextf("Number of clusters: %i", clustersNumber, domain = "R-prevR")
    )
    message(
      gettextf(
        "Number of observations: %i",
        ObservationNumber,
        domain = "R-prevR"
      )
    )
    message(
      gettextf(
        "Number of positive cases: %i",
        PositiveCases,
        domain = "R-prevR"
      )
    )
    if (isWeightedData) {
      message("The dataset is weighted.", domain = "R-prevR")
    } else {
      message("The dataset is not weighted.", domain = "R-prevR")
    }
    message(
      gettextf(
        "\nNational prevalence: %.2f%%",
        nationalPrev,
        domain = "R-prevR"
      )
    )
    if (isWeightedData) {
      message(
        gettextf(
          "National weighted prevalence: %.2f%%",
          weightedNationalPrev,
          domain = "R-prevR"
        )
      )
    }
    message(gettextf("\nProjection used: %s", proj, domain = "R-prevR"))
    message("\nCoordinate range", domain = "R-prevR")
    print(coordinatesRange)
    if (attr(boundary, "valid")) {
      message("\nBoundary coordinate range", domain = "R-prevR")
      print(boundaryCoordinatesRange)
    }
    if (is.prevR(object, "rings")) {
      rings <- slot(object, "rings")
      N <- sapply(rings, function(x) x$N)
      R <- sapply(rings, function(x) x$R)
      couples <- cbind(N = N, R = R)
      dimnames(couples) <- list(seq_len(nrow(couples)), c("N", "R"))
      message(
        "\nAvailable (N,R) couples in the slot 'rings':",
        domain = "R-prevR"
      )
      print(as.data.frame(couples), row.names = FALSE)
    }
  }
)
