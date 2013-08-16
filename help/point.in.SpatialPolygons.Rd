\encoding{utf8}
\name{point.in.SpatialPolygons}
\alias{point.in.SpatialPolygons}
\title{Test if a point falls within a given polygon.}
\description{This function tests if one or more points are located inside an object of class \code{\link[sp:SpatialPolygons-class]{SpatialPolygons}}.}
\usage{point.in.SpatialPolygons(point.x, point.y, SpP)}
\arguments{
\item{point.x}{numerical array of x-coordinates of points.}
\item{point.y}{numerical array of y-coordinates of points.}
\item{SpP}{object of class \code{\link[sp:SpatialPolygons-class]{SpatialPolygons}}}
}
\value{Boolean array.}

\seealso{\code{\link[sp]{point.in.polygon}}\{\pkg{sp}\}, \code{\link{NA.outside.SpatialPolygons}}.}
\keyword{manip}
\keyword{spatial}