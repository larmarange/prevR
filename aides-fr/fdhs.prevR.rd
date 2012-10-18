\encoding{utf8}
\name{fdhs}
\alias{fdhs}
\alias{fdhs.boundary}
\alias{fdhs.clusters}
\docType{data}
\title{Données issues d'une simulation d'EDS.}

\description{Jeu de données issu d'une simulation d'Enquête Démographique et de Santé (EDS) sur un pays fictif présentant une prévalence nationale de 10 \% avec 8000 personnes enquêtées réparties en 401 grappes. Il contient trois objets :\itemize{
\item \code{fdhs.clusters} : tableau de données aggrégées par grappe.
\item \code{fdhs.boundary} : objet de classe \code{\link[sp:SpatialPolygons-class]{SpatialPolygons}} définissant les frontières du pays fictif.
\item \code{fdhs} : objet de classe \code{\link[=prevR-class]{prevR}} basé sur les 2 jeux de données précédents et obtenu avec \code{\link{as.prevR}}.
}}

\usage{fdhs}

\references{
Larmarange J., Vallo R., Yaro S., Msellati P., Meda N., Ferry B. (2011) "Méthodes pour cartographier les tendances régionales de la prévalence du VIH à partir des Enquêtes Démographiques et de Santé (EDS)", \emph{Cybergeo: European Journal of Geography}, \url{http://cybergeo.revues.org/}.\cr

Larmarange J. (2007) \emph{Prévalences du VIH en Afrique : validité d'une mesure}, thèse de doctorat en démographie, sous la direction de Benoît Ferry, université Paris Descartes, \url{http://tel.archives-ouvertes.fr/tel-00320283}.\cr

Larmarange J., Vallo R., Yaro S., Msellati P., Meda N., Ferry B. (2006), "Cartographier les données des enquêtes démographiques et de santé à partir des coordonnées des zones d'enquête", \emph{Chaire Quételet, 29 novembre au 1er décembre 2006}, Université Catholique de Louvain, Louvain-la-Neuve, Belgique, \url{http://www.uclouvain.be/13881.html}.
}

\examples{
str(fdhs)
str(fdhs.clusters)
str(fdhs.boundary)
demo(prevR)
}
\keyword{datasets}