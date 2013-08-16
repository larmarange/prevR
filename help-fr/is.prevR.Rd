\encoding{utf8}
\name{is.prevR}
\alias{is.prevR}
\title{Tester si un objet est de classe prevR.}

\description{Cette fonction permet de tester si un objet est de classe \code{\link[=prevR-class]{prevR}}. Elle permet aussi de tester la présence des éléments \code{rings} et \code{boundary}.}

\usage{is.prevR(object, slot = NULL)}

\arguments{
\item{object}{un objet quelconque.}
\item{slot}{ un vecteur de chaînes de caractères pouvant contenir "clusters", "rings","boundary" ou "proj".}
}

\details{
Attention les éléments \code{rings} et \code{boundary} sont toujours présents dans un objet de classe \code{\link[=prevR-class]{prevR}}, mais \code{rings} peut être une liste \code{NULL} et \code{boundary} un \code{\link[sp:SpatialPolygons-class]{SpatialPolygons}} avec un attribut \code{valid} posé à \code{FALSE} (lorsque les limites de la zone d'étude n'ont pas été définies explicitement).
\itemize{
  \item Si \code{rings} est une liste \code{NULL}, \code{is.prevR(object,"rings")} renverra \code{FALSE}.
  \item Si \code{boundary} a un attribut \code{valid} égal à \code{FALSE}, \code{is.prevR(object,"boundary")} renverra \code{FALSE}.
} 
}

\value{
  Cette fonction renvoie un vecteur logique.
}

\seealso{
  \code{\link{prevR-class}}.
}

\examples{
col <- c(id = "cluster", 
         x = "x",
         y="y",
         n="n",
         pos = "pos",
         c.type = "residence",
         wn="weighted.n",
         wpos="weighted.pos"
        )
dhs <- as.prevR(fdhs.clusters,col, fdhs.boundary)

is.prevR(dhs)
is.prevR(dhs,"rings")
is.prevR(dhs,"boundary")

dhs <- rings(dhs,N=300)
is.prevR(dhs,"rings")
}
\keyword{classes}