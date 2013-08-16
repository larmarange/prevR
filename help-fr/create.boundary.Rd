\encoding{utf8}
\name{create.boundary} 
\alias{create.boundary}
\title{Fournit les frontières d'un pays.}

\description{Cette fonction embarque le jeu de données \code{\link{TMWorldBorders}}. Elle permet de sélectionner un pays et le renvoie sous la forme d'un objet de classe \code{\link[sp:SpatialPolygons-class]{SpatialPolygons}}.}

\usage{create.boundary(countries = NULL, multiple = F, proj = "+proj=longlat")}

\arguments{
\item{countries}{un vecteur de chaînes de caratères contenant le nom des pays que vous désirez extraire du jeu de données. Si \code{NULL}, une boite de dialogue sera affichée pour sélectionner le pays désiré.}
\item{multiple}{la boîte de dialogue doit-elle permettre de sélectionner plusieurs pays (inutilisé si \code{countries} est fourni) ?}
\item{proj}{projection dans laquelle le résultat doit être exprimé (en longitude/latitude par défaut).}
}

\details{\code{proj} peut être une chaîne de caractères correspondant à une projection \emph{PROJ.4} (voir \url{http://trac.osgeo.org/proj/} pour plus de détails) ou un objet de classe \code{\link[sp:CRS-class]{CRS}}\{\pkg{sp}\}.}

\value{Objet de classe \code{\link[sp:SpatialPolygons-class]{SpatialPolygons}}\{\pkg{sp}\}.}

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