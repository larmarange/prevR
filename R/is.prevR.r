#' Test if an object is of class prevR.
#' This function test if the class of an object is [prevR-class].
#' It could be used to test the slot `rings` or the slot `boundary`.
#'
#' @param object object to test.
#' @param slot "clusters", "rings","boundary" or "proj".
#' @details
#' Slots `rings` and `boundary` are always present in an object of
#' class [prevR-class], but `rings` could be `NULL` and
#' `boundary` a [sf::sf] object with an attribute named `valid`
#' with the value `FALSE` (when boundaries of the studied
#' area have not been specified explicitly).
#' \itemize{
#'  \item If `rings` is `NULL`, `is.prevR(object,"rings")` will return `FALSE`.
#'  \item If `boundary` has an attribute `valid` equal to `FALSE`,
#'   `is.prevR(object,"boundary")` will return `FALSE`.
#' }
#'
#' @return  `TRUE` or `FALSE`
#' @seealso [prevR-class].
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
#' is.prevR(dhs)
#' is.prevR(dhs, "rings")
#' is.prevR(dhs, "boundary")
#'
#' dhs <- rings(dhs, N = 300)
#' is.prevR(dhs, "rings")
#'
#' @keywords class
#' @export

is.prevR <- function(object, slot = NULL) {
  if (!inherits(object, "prevR")) {
    return(FALSE)
  }
  if (is.null(slot) && inherits(object, "prevR")) {
    return(TRUE)
  }
  ind <- match(slot, slotNames(object), nomatch = 0)
  if (any(ind == 0)) {
    for (i in length(slot[ind == 0])) {
      message(
        gettextf(
          "The slot '%s' doesn't exist for class 'prevR'.",
          slot[ind == 0][i],
          domain = "R-prevR"
        )
      )
    }
    return(FALSE)
  }
  if (!is.null(slot)) {
    response <- NULL
    for (one.slot in slot) {
      if (one.slot == "boundary") {
        if (attr(slot(object, "boundary"), "valid")) {
          response <- c(response, TRUE)
        } else {
          response <- c(response, FALSE)
        }
      } else {
        if (length(slot(object, one.slot)) > 0) {
          response <- c(response, TRUE)
        } else {
          response <- c(response, FALSE)
        }
      }
    }
    names(response) <- slot
  }
  response
}
