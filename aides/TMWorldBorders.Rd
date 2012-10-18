\encoding{utf8}
\name{TMWorldBorders}
\alias{TMWorldBorders}
\docType{data}
\title{Dataset "TM World Borders Dataset 0.3".}

\description{
This dataset provides boundaries of all countries in the world, in decimal degrees. Available variables are:\itemize{
\item "FIPS" FIPS 10-4 Country Code.
\item "ISO2" ISO 3166-1 Alpha-2 Country Code.
\item "ISO3" ISO 3166-1 Alpha-3 Country Code.
\item "UN" ISO 3166-1 Numeric-3 Country Code.
\item "NAME" Name of country/area.
\item "AREA" Land area, FAO Statistics (2002).
\item "POP2005" Population, World Population Prospects (2005).
\item "REGION" Macro geographical (continental region), UN Statistics.
\item "SUBREGION" Geographical sub-region, UN Statistics.
\item "LON" Longitude.
\item "LAT" Latitude.
}}

\usage{TMWorldBorders}

\format{Object of class \code{\link[sp:SpatialPolygonsDataFrame-class]{SpatialPolygonsDataFrame}}.}

\source{
Provided by Bjorn Sandvik on \url{http://thematicmapping.org/downloads/world_borders.php}. The dataset was derived by Schuyler Erle from public domain sources. Sean Gilles did some clean up and made some enhancements. The dataset is available under a \emph{Creative Commons Attribution-Share Alike License} (\url{http://creativecommons.org/licenses/by-sa/3.0/}).
}

\note{The boundaries, names designations used do not imply official endorsement or acceptance by the authors. Use this dataset with care, as several of the borders are disputed.}

\examples{
plot(TMWorldBorders)
}

\keyword{datasets}
\keyword{spatial}