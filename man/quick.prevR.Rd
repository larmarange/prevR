% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/quick.prevR.r
\name{quick.prevR}
\alias{quick.prevR}
\title{Quick prevR analysis and plot}
\usage{
quick.prevR(
  object,
  N = Noptim(object),
  nb.cells = 100,
  cell.size = NULL,
  weighted = NULL,
  plot.results = TRUE,
  return.results = FALSE,
  return.plot = FALSE,
  legend.title = "\%",
  cex = 0.7,
  progression = TRUE
)
}
\arguments{
\item{object}{object of class \linkS4class{prevR}.}

\item{N}{integer or list of integers corresponding to the rings to use.}

\item{nb.cells}{number of cells on the longest side of the studied area
(unused if \code{cell.size} is defined).}

\item{cell.size}{size of each cell (in the unit of the projection).}

\item{weighted}{use weighted data (TRUE, FALSE or "2")?}

\item{plot.results}{plot the results?}

\item{return.results}{return the results?}

\item{return.plot}{return the plot within the results?}

\item{legend.title}{title of the legend}

\item{cex}{to control the text size on the graph}

\item{progression}{show a progress bar?}
}
\value{
A list of one or several elements, depending on the arguments:
(i) \code{prev} is a \code{SpatialPixelsDataFrame} containing the prevalence
surface; (ii) \code{radius} a \code{SpatialPixelsDataFrame} containing the
kriged surface of the rings radii; (iii) \code{plot} a \code{ggplot} graph.
}
\description{
This function performs several analysis in one go:
(i) apply \code{\link[=rings]{rings()}};
(ii) compute prevalence surface with \code{\link[=kde]{kde()}};
(iii) compute the surface of rings radii with \code{\link[=krige]{krige()}};
(iv) plot prevalence surface using \code{\link[=prevR.colors.red]{prevR.colors.red()}} and add rings radii
as a contour plot.
}
\details{
\code{N} determine the rings to use for the estimation.
By default, a suggested value of N will be computed with \code{\link[=Noptim]{Noptim()}}.
}
\examples{
\dontrun{
quick.prevR(fdhs)
}

}
\seealso{
\code{\link[=Noptim]{Noptim()}}, \code{\link[=rings]{rings()}}, \code{\link[=kde]{kde()}} and \code{\link[=krige]{krige()}}.
}
\keyword{plot}
\keyword{smooth}
\keyword{spatial}
