\encoding{utf8}
\name{point.in.SpatialPolygons}
\alias{point.in.SpatialPolygons}
\title{Détermine si un point appartient à un polygone.}
\description{Cette fonction teste si un point ou plusieurs points sont situés à l'intérieur d'un objet \code{\link[sp:SpatialPolygons-class]{SpatialPolygons}}.}
\usage{point.in.SpatialPolygons(point.x, point.y, SpP)}
\arguments{
\item{point.x}{   coordonnées en x des points à tester.}
\item{point.y}{   coordonnées en y des points à tester.}
\item{SpP}{objet \code{\link[sp:SpatialPolygons-class]{SpatialPolygons}}}
}
\value{Renvoie un vecteur logique.}

\seealso{\code{\link[sp]{point.in.polygon}}\{\pkg{sp}\}, \code{\link{NA.outside.SpatialPolygons}}.}
\keyword{manip}
\keyword{spatial}