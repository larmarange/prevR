.isInputOk.prevR <- function(...) {
  N <- list(...)$N
  if (!is.null(N) && !is.numeric(N)) {
    stop("N is not numeric.", call. = FALSE)
  }
  R <- list(...)$R
  if (!is.null(R) && !is.numeric(R)) {
    stop("R is not numeric.", call. = FALSE)
  }
  nc <- sapply(list(...), length)
  if ((sum(nc == 1) + sum(nc == max(nc))) == length(nc)) {
    return(NULL)
  }
  if (all(nc == nc[[1]])) {
    return(NULL)
  }
  if (sum(nc != 1) != 1) {
    nom.param <- paste(names(nc), sep = ", ")
    stop(
      gettextf(
        "the input parameters are not valid: %s must have the same length or a length equal to 1.", #nolint
        nom.param,
        domain = "R-prevR"
        ),
      call. = FALSE
    )
  }
  NULL
}
