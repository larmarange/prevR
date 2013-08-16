\encoding{utf8}
\name{export,prevR-method}
\alias{export-methods}
\alias{export,prevR-method}
\alias{export}
\docType{methods}
\title{Exporte un objet prevR.}

\description{Cette méthode permet d'exporter un objet de classe \code{\link[=prevR-class]{prevR}} dans différents formats.}

\usage{
export(object, element, 
       format, file,
       N = NULL, R = NULL, clusters.only = FALSE,
       ext = NULL, sep = NULL, dec = NULL, ...
      )
}

\arguments{
\item{object}{objet de classe \code{\link[=prevR-class]{prevR}}.}
\item{element}{élément à exporter : "clusters" ou "boundary".}
\item{format}{format d'exportation : "dbf", "txt", csv", "csv2" ou "shp" (inutilisé si \code{element="boundary"}).} 
\item{file}{nom du fichier à créer (sans extension).}
\item{N}{entier ou liste d'entiers indiquant les éléments de \code{rings} à extraire (inutilisé si \code{element="boundary"}).}
\item{R}{entier ou liste d'entiers indiquant les éléments de \code{rings} à extraire (inutilisé si \code{element="boundary"}).}
\item{clusters.only}{renvoyer uniquement l'élément \code{clusters} de \code{object} (inutilisé si \code{element="boundary"})?}
\item{ext}{forcer l'extension du fichier créé (inutilisé si \code{element="boundary"} ou si \code{format="shp"}).}
\item{sep}{forcer le séparateur de champs (inutilisé si \code{element="boundary"} ou si \code{format="shp"} ou si \code{format="dbf"}).}
\item{dec}{forcer le séparateur de décimal. (inutilisé si \code{element="boundary"} ou si \code{format="shp"} ou si \code{format="dbf"}).}
\item{\dots}{arguments additionnels transmis à \code{\link[maptools]{writePolyShape}}, \code{\link[maptools]{writePointsShape}}, \code{\link[foreign]{write.dbf}} ou \code{\link[utils]{write.table}}.}
}

\details{
Si \code{element="boundary"}, l'élément \code{boundary} est exporté au format \emph{shapefile}.

Sinon, c'est l'élément \code{clusters}, fusionné avec \code{rings}, qui sera fusionné.\cr Voir \code{\link{as.data.frame.prevR}} pour l'utilisation des paramètres \code{N}, \code{R} et \code{clusters.only}.\cr
\code{format} permet de spécifier le format d'export du tableau de données obtenu\cr avec \code{\link{as.data.frame.prevR}} :
\tabular{ll}{
  "shp" \tab Shape File\cr
  "dbf" \tab DBASE format (extension: .dbf)\cr
  "txt" \tab texte tabulé (extension: .txt)\cr
  "csv" \tab 'comma separated values' (extension : .csv)\cr
  "csv2" \tab Variante de CSV utilisant un point-virgule (extension: .csv)
}
\code{ext} permet de forcer l'extension du fichier produit, excepté pour les exports en \emph{shapefile} qui produisent systématiquement trois fichiers (.shp, .shx et .dbf).

Le format "txt" utilise par défaut une tabulation comme séparateur et un point pour indiquer les décimales. Le format "csv" utilise une virgule comme séparateur et un point pour les décimales. Le format "csv2" est une variante utilisant le point-virgule comme séparateur et la virgule pour les décimales, convention utilisée par certaines versions d'Excel (notamment la version française). \code{sep} et \code{dec} permettent de forcer le séparateur et le point décimal à utiliser (en conjonction avec le format "txt").
}

\section{Methods}{\describe{\item{\code{signature(object = "prevR")}}{}}}

\seealso{\code{\link[maptools]{writePolyShape}} \{\pkg{maptools}\}, \code{\link[maptools]{writePointsShape}} \{\pkg{maptools}\}, \code{\link[foreign]{write.dbf}} \{\pkg{foreign}\}, \code{\link[utils]{write.table}} \{\pkg{utils}\}.}

\examples{
\dontrun{
export(fdhs, element="boundary", file="area")
export(fdhs, element="clusters", format="shp", file="points")

dhs <- rings(fdhs,N=c(100,300,500))
export(dhs, element="clusters", format="csv", N=300, file="points")
}
}
\keyword{manip}
\keyword{spatial}