\encoding{utf8}
\name{import.dhs}
\alias{import.dhs}
\title{Importer les données d'une EDS dans prevR.}

\description{Cette fonction guide l'utilisateur pas à pas pour importer les données d'une Enquête Démographique et de Santé (EDS) et créer un objet de classe \code{\link[=prevR-class]{prevR}}.}

\usage{import.dhs(file.sav, file.dbf)}

\arguments{
\item{file.sav}{fichier contenant les données individuelles d'une Enquête Démographique et de Santé (EDS), téléchargé au format SPSS (.sav) sur \url{http://www.measuredhs.com}.}
\item{file.dbf}{fichier contenant les données GPS d'une Enquête Démographique et de Santé (EDS), téléchargé au format DATABASE (.dbf) sur \url{http://www.measuredhs.com}.}
}

\note{
Si vous ne précisez pas le chemin des fichiers, \R inspectera le dossier de travail (voir \code{\link[base]{setwd}}). Pour spécifier le chemin des fichiers, voir \code{\link[base]{file.path}}.

Cette fonction est destinée spécifiquement à l'importation des Enquêtes Démograhiques et de Santé. Pour créer un objet de classe \code{\link[=prevR-class]{prevR}} de manière plus générique, voir \code{\link{as.prevR}}.
}

\seealso{\code{\link{as.prevR}}, \code{\link{prevR-class}}.}

\examples{
\dontrun{
import.dhs("data.sav", "gps.dbf")
}
}
\keyword{manip}