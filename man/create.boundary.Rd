\encoding{utf8}
\name{create.boundary} 
\alias{create.boundary}
\title{Provide national boundaries of a country.}

\description{This function uses the data set \code{\link{TMWorldBorders}}. One or several countries can be selected and will be returned as an object of class \code{\link[sp:SpatialPolygons-class]{SpatialPolygons}}.}

\usage{create.boundary(countries = NULL, multiple = F, proj = "+proj=longlat")}

\arguments{
\item{countries}{a vector of character string corresponding to the name of the countries you want to extract from the dataset. If \code{NULL}, a dialog box will be appear in order to selc the desired country.}
\item{multiple}{should the dialog box allow multiple selection (unused if \code{countries} is specified)?}
\item{proj}{map projection to use for the result (longitude and latitude in decimal degrees by default).}
}

\details{
\code{proj} could be a character string corresponding to a \emph{PROJ.4} projection (see \url{http://trac.osgeo.org/proj/} for more details) or an object of class \code{\link[sp:CRS-class]{CRS}}\{\pkg{sp}\}.
}

\value{Object of class \code{\link[sp:SpatialPolygons-class]{SpatialPolygons}}\{\pkg{sp}\}.}

\note{
The result will be automatically plotted.
}

\seealso{\code{\link{TMWorldBorders}}.}

\examples{
\dontrun{
boundary <- create.boundary()
}
\dontshow{par(ask = TRUE)}
boundary <- create.boundary("Burkina Faso")
boundary <- create.boundary("Burkina Faso",
            proj="+proj=utm +zone=30 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
boundary <- create.boundary(countries = c("Burkina Faso", "Ghana", "Benin"))
\dontshow{par(ask = FALSE)}
}
\keyword{manip}
\keyword{spatial}