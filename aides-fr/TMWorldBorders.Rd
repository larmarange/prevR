\encoding{utf8}
\name{TMWorldBorders}
\alias{TMWorldBorders}
\docType{data}
\title{Jeu de données "TM World Borders Dataset 0.3".}

\description{
Ce jeu de données contient les frontières de l'ensemble des pays du monde, exprimées en longitude/latitude. Les attributs disponibles sont :\itemize{
\item "FIPS" code pays FIPS 10-4.
\item "ISO2" code pays ISO 3166-1 Alpha-2.
\item "ISO3" code pays ISO 3166-1 Alpha-3.
\item "UN" code pays ISO 3166-1 Numeric-3.
\item "NAME" nom du pays/zone (en anglais).
\item "AREA" superficie des terres, FAO Statistics (2002).
\item "POP2005" population, World Population Prospects (2005).
\item "REGION" région continentale, UN Statistics.
\item "SUBREGION" sous-région, UN Statistics.
\item "LON" longitude.
\item "LAT" latitude.
}}

\usage{TMWorldBorders}

\format{Objet de classe \code{\link[sp:SpatialPolygonsDataFrame-class]{SpatialPolygonsDataFrame}}.}

\source{Le jeu de données \emph{World Borders Dataset 0.3} a été élaboré par Bjorn Sandvik, Schuyler Erle et Sean Gilles à partir de sources dans le domaine publique. Ce jeu de données, disponible sur \url{http://thematicmapping.org/downloads/world_borders.php}, est placé sous licence \emph{Creative Commons Attribution-Share Alike} (\url{http://creativecommons.org/licenses/by-sa/3.0/}).}
\note{Les frontières et désignations employées n'impliquent l'expression d'aucune opinion ni acceptation de la part des auteurs.}

\examples{
plot(TMWorldBorders)
}

\keyword{datasets}
\keyword{spatial}