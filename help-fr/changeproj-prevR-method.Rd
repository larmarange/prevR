\encoding{utf8}
\name{changeproj,prevR-method}
\alias{changeproj,prevR-method}
\alias{changeproj}
\alias{changeproj-methods}
\docType{methods}
\title{Changer la projection d'un objet prevR.}

\description{Cette fonction permet de modifier la projection d'un objet \code{\link[=prevR-class]{prevR}}.}

\usage{\S4method{changeproj}{prevR}(object, proj)}

\arguments{
\item{object}{objet de classe \code{\link[=prevR-class]{prevR}}.}
\item{proj}{projection de destination.}
}

\details{
\code{proj} peut être une chaîne de caractères correspondant à une projection \emph{PROJ.4} (voir \url{http://trac.osgeo.org/proj/} pour plus de détails) ou un objet de classe \code{\link[sp:CRS-class]{CRS}}\{\pkg{sp}\}.

\code{changeproj} modifie les colonnes "x" et "y" de l'élément \code{clusters} de \code{object} et projette \code{boundary} dans le nouveau système de coordonnées.\cr
Le cas échéant, l'élément \code{rings} est recalculé.
}

\value{Renvoie \code{object} exprimé dans la projection \code{proj}.}

\seealso{\code{\link[rgdal]{spTransform}}\{\pkg{rgdal}\}, \code{\link{prevR-class}}.} 

\examples{
print(fdhs)
plot(fdhs,axes=TRUE,main="Projection : longitude/latitude")

fdhs2 <- changeproj(fdhs,
           "+proj=utm +zone=30 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
print(fdhs2)
dev.new()
plot(fdhs2,axes=TRUE, main="Projection: UTM Zone 30")
}
\keyword{manip}
\keyword{spatial}