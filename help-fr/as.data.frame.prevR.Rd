\encoding{utf8}
\name{as.data.frame.prevR}
\alias{as.data.frame.prevR}
\alias{as.data.frame}
\title{Convertir un objet prevR en tableau de données.}
\description{Cette fonction permet de fusionner les éléments \code{clusters} et \code{rings} d'un objet \code{\link[=prevR-class]{prevR}} en un unique tableau de données.}
\usage{\S3method{as.data.frame}{prevR}(x, ..., N = NULL, R = NULL, clusters.only = FALSE)}
\arguments{
\item{x}{objet de classe \code{\link[=prevR-class]{prevR}} à exporter.}
\item{\dots}{non utilisé, pour compatibilité avec la méthode générique \code{\link[base]{as.data.frame}}.}
\item{N}{entier ou liste d'entiers indiquant les éléments de \code{rings} à extraire.}
\item{R}{entier ou liste d'entiers indiquant les éléments de \code{rings} à extraire.}
\item{clusters.only}{renvoyer uniquement l'élément \code{clusters} de \code{x} ?}
}

\value{
Si \code{clusters.only = TRUE}, la fonction renverra simplement l'élément \code{clusters} de \code{x}.

Sinon, les éléments \code{clusters} et \code{rings} de \code{x} seront fusionnés en un seul tableau de données. Les variables de \code{rings} seront renommées en leur ajoutant un suffixe de la forme \emph{.N300.RInf}.

\code{N} et \code{R} permettent de sélectionner les éléments de \code{rings} à exporter. S'ils ne sont pas spécifiés (valeur \code{NULL}), tous les éléments de \code{rings} seront inclus.
}

\seealso{\code{\link[base]{as.data.frame}}\{\pkg{base}\}, \code{\link[prevR]{prevR-class}}.}
\examples{
r.fdhs <- rings(fdhs,N=c(100,200,300))
str(r.fdhs)
str(as.data.frame(r.fdhs,clusters.only=TRUE))
str(as.data.frame(r.fdhs))
str(as.data.frame(r.fdhs,N=300))
}
\keyword{manip}