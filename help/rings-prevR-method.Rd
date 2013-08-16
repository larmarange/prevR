\encoding{utf8}
\name{rings,prevR-method}
\docType{methods}
\alias{rings}
\alias{rings-methods}
\alias{rings,prevR-method}
\title{Calculation of rings of equal number of observation and/or equal radius.}

\description{For each cluster, this function determines a ring of equal number of observations and/or equal radius and calculates several indicators from observations located inside that ring.}

\usage{rings(object, N = seq(100, 500, 50), R = Inf, progression = TRUE)}

\arguments{
\item{object}{object of class \code{\link[=prevR-class]{prevR}}.}
\item{N}{minimum number of observations.}
\item{R}{maximum rings radius (in kilometers if coordinates in decimal degrees, in the unit of the projection otherwise).}
\item{progression}{show a progress bar?}
}

\details{
Pour each ligne of the data frame \code{clusters} of \code{object}, \code{rings} determines a ring, centred on the cluster. It could be:\itemize{
\item rings of eaqul number of observations if \code{N} is finite and \code{R=Inf};
\item rings of equal radius if \code{N=Inf} and \code{R} is finite;
\item a combination of both (see below) if \code{N} and \code{R} are finite.
}
For \emph{rings of equal number of observations}, \code{rings} selects the smallest ring containing at least \code{N} valid observations.\cr
For \emph{rings of equal radius}, \code{rings} selects all clusters located at a lower distance than \code{R} from the central cluster.\cr
For \emph{combination of both}, \code{rings} calculates firts the ring with the minimum number of observations and test if its radius is lower than \code{R} or not. If so, the ring is kept, otherwise the ring of maximum radius is calculated.

Different series of rings could be simultaneoulsy calculated by providing different values for \code{N} and \code{R}. \code{rings} will calculate rings corresponding to each couple (N,R).
}
\value{
Return \code{object} with the slot \code{rings} completed for each couple (N,R).

Each entry is composed of 3 elements: \code{N}, minimum number of observations per ring; \code{R}, maximum radius of rings and \code{estimates}, a data frame with the following variables:\itemize{
\item "id" cluster ID.
\item "r.pos" number of positive cases inside the ring.
\item "r.n" number of valid observations inside the ring.
\item "r.prev" observed prevalence (in \%) inside the ring (r.pos/r.n).
\item "r.radius" ring radius (in kilometers if coordinates in decimal degrees, in the unit of the projection otherwise).
\item "r.clusters" number of clusters located inside the ring.
\item "r.wpos" (optional) sum of weights of positive cases inside the ring.
\item "r.wn" (optional) sum of weights of valid observations inside the ring.
\item "r.wprev" (optional) weighted observed prevalence (in \%) inside the ring (r.wpos/r.wn).
}
Note: the list \code{rings} is named, the name of each element is N\emph{N_value}.R\emph{R_value}, for example \emph{N300.RInf}. 

Note 2: \emph{r.wpos}, \emph{r.wn} and \emph{r.wprev} are calculated only if the slot \code{clusters} of \code{object} contains weighted data.
}

\seealso{\code{\link{prevR-class}}.}

\references{
Larmarange Joseph, Vallo Roselyne, Yaro Seydou, Msellati Philippe and Meda Nicolas (2011) "Methods for mapping regional trends of HIV prevalence from Demographic and Health Surveys (DHS)", \emph{Cybergeo : European Journal of Geography}, no 558, \url{http://cybergeo.revues.org/24606}, DOI: 10.4000/cybergeo.24606.

Larmarange Joseph (2007) \emph{Prévalences du VIH en Afrique : validité d'une mesure}, PhD thesis in demography, directed by Benoît Ferry, université Paris Descartes, \url{http://tel.archives-ouvertes.fr/tel-00320283}.

Larmarange Joseph, Vallo Roselyne, Yaro Seydou, Msellati Philippe Meda Nicolas and Ferry Benoît (2006), "Cartographier les données des enquêtes démographiques et de santé à partir des coordonnées des zones d'enquête", \emph{Chaire Quételet, 29 novembre au 1er décembre 2006}, Université Catholique de Louvain, Louvain-la-Neuve, Belgique, \url{http://www.uclouvain.be/13881.html}.
}

\examples{\dontrun{
print(fdhs)
dhs <- rings(fdhs,N=c(100,200,300,400,500))
print(dhs)
}}
\keyword{math}
\keyword{spatial}