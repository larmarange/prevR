\encoding{utf8}
\name{changeproj,prevR-method}
\alias{changeproj,prevR-method}
\alias{changeproj}
\alias{changeproj-methods}
\docType{methods}
\title{Convert map projection of a object of class prevR.}

\description{This function converts map projection (and/or datum) used by an object of class \code{\link[=prevR-class]{prevR}} into another one.}

\usage{\S4method{changeproj}{prevR}(object, proj)}

\arguments{
\item{object}{object of class \code{\link[=prevR-class]{prevR}}.}
\item{proj}{new map projection.}
}

\details{
\code{proj} could be a character string corresponding to a \emph{PROJ.4} projection (see \url{http://trac.osgeo.org/proj/} for more details) or an object of class \code{\link[sp:CRS-class]{CRS}}\{\pkg{sp}\}.

\code{changeproj} transform the columns  "x" and "y" of the slot \code{clusters} of \code{object} and convert \code{boundary} using the new map projection defined by \code{proj}.\cr
If applicable, the slot \code{rings} will be recalculated.
}

\value{Return \code{object} expressed in the projection \code{proj}.}

\seealso{\code{\link[rgdal]{spTransform}}\{\pkg{rgdal}\}, \code{\link{prevR-class}}.} 

\examples{
print(fdhs)
plot(fdhs, axes=TRUE, main="Projection: longitude/latitude")

fdhs2 <- changeproj(fdhs,
         "+proj=utm +zone=30 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
print(fdhs2)
dev.new()
plot(fdhs2, axes=TRUE, main="Projection: UTM Zone 30")
}
\keyword{manip}
\keyword{spatial}