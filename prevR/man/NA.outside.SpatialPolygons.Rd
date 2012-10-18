\encoding{UTF-8}
\name{NA.outside.SpatialPolygons}
\alias{NA.outside.SpatialPolygons}

\title{Affect NA value to points located outside a polygon.}

\description{This function forces points of an object of class \code{\link[sp:SpatialPixelsDataFrame-class]{SpatialPixelsDataFrame}} located outside the limits defined by an object of class \code{\link[sp:SpatialPolygons-class]{SpatialPolygons}} to \code{NA}.}

\usage{NA.outside.SpatialPolygons(sp.data, sp.poly)}

\arguments{
\item{sp.data}{object of class \code{\link[sp:SpatialPixelsDataFrame-class]{SpatialPixelsDataFrame}}.}
\item{sp.poly}{objec of class \code{\link[sp:SpatialPolygons-class]{SpatialPolygons}}.}
}

\value{Return \code{sp.data} modified.}

\seealso{\code{\link{point.in.SpatialPolygons}}.}

\keyword{manip}
\keyword{spatial}