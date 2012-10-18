\encoding{UTF-8}
\name{NA.outside.SpatialPolygons}
\alias{NA.outside.SpatialPolygons}

\title{Affecter une valeur manquante aux points situés en-dehors d'un polygone.}

\description{Cette fonction affecte la valeur \code{NA} aux points d'un objet de classe \code{\link[sp:SpatialPixelsDataFrame-class]{SpatialPixelsDataFrame}} situés en dehors d'un objet \code{\link[sp:SpatialPolygons-class]{SpatialPolygons}}.}

\usage{NA.outside.SpatialPolygons(sp.data, sp.poly)}

\arguments{
\item{sp.data}{objet de classe \code{\link[sp:SpatialPixelsDataFrame-class]{SpatialPixelsDataFrame}}.}
\item{sp.poly}{objet \code{\link[sp:SpatialPolygons-class]{SpatialPolygons}}.}
}

\value{Renvoie \code{sp.data} modifié.}

\seealso{\code{\link{point.in.SpatialPolygons}}.}

\keyword{manip}
\keyword{spatial}