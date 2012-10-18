\encoding{utf8}
\name{as.SpatialGrid,prevR-method}
\alias{as.SpatialGrid}
\alias{as.SpatialGrid-methods}
\alias{as.SpatialGrid,prevR-method}
\docType{methods}
\title{Création d'une grille à partir d'un objet prevR.}

\description{Cette fonction permet de calculer une grille de points régulièrement répartis à partir de l'élément \code{boundary} d'un objet \code{\link[=prevR-class]{prevR}}. Elle est notamment appelée en interne par les méthodes \code{\link[=kde,prevR-method]{kde}}, \code{\link[=krige,prevR-method]{krige}} et \code{\link[=idw,prevR-method]{idw}}.}

\usage{as.SpatialGrid(object, nb.cells = 100, cell.size = NULL)}

\arguments{
\item{object}{objet de classe \code{\link[=prevR-class]{prevR}}.}
\item{nb.cells}{nombre de cellules sur la plus grande dimension (inutilisé si \code{cell.size} est défini).}
\item{cell.size}{côté d'une cellule (dans l'unité de la projection).}
}

\details{Cette fonction crée une grille de points régulièrement espacés, chaque cellule étant carrée et ayant un côté égal à \code{cell.size}. Si \code{cell.size} n'est pas défini, le côté des cellules sera défini en divisant le plus grand côté de l'élément \code{boundary} de \code{object} par \code{nb.cells}.}

\value{Objet de classe \code{\link[sp:SpatialGrid-class]{SpatialGrid}}\{\pkg{sp}\}.}

\seealso{\code{\link[sp]{GridTopology}}\{\pkg{sp}\}, \code{\link[sp]{SpatialGrid-class}}\{\pkg{sp}\}.}

\examples{
str(as.SpatialGrid(fdhs))
str(as.SpatialGrid(fdhs, nb.cells=200))
}
\keyword{manip}
\keyword{spatial}