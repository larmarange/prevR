#' Summary of a prevR object.
#'
#' Method \code{print} for objects of class [prevR-class]:
#' shows a summary of the object's characteristics.
#'
#' @param x object of class [prevR-class].
#'
#' @note Exactly the same as [show()].
#' @seealso [summary()].
#' @examples
#' print(fdhs)
#' \dontrun{
#' dhs <- rings(fdhs, N = c(100, 300, 500))
#' print(dhs)
#' }
#'
#' @aliases print print-methods print,prevR-method
#' @exportMethod print

setMethod(
  "print", "prevR",
  function(x) {
    show(x)
    invisible(NULL)
  }
)
