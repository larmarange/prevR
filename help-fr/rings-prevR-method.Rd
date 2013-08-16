\encoding{utf8}
\name{rings,prevR-method}
\docType{methods}
\alias{rings}
\alias{rings-methods}
\alias{rings,prevR-method}
\title{Calculer des cercles de même effectif et/ou de même rayon.}

\description{Cette fonction détermine pour chaque grappe un cercle de même effectif et/ou de même rayon et calcule plusieurs indicateurs à partir des observations situées dans le cercle.}

\usage{rings(object, N = seq(100, 500, 50), R = Inf, progression = TRUE)}

\arguments{
\item{object}{objet de classe \code{\link[=prevR-class]{prevR}}.}
\item{N}{effectif minimum des cercles.}
\item{R}{rayon maximum des cercles (en kilomètres si \code{object} est exprimé en longitude/latitude, dans l'unité de la projection sinon).}
\item{progression}{afficher une barre de progression des calculs ?}
}

\details{
Pour chaque grappe de l'élément \code{clusters} de \code{object}, \code{rings} détermine un cercle, centré sur la grappe. Il peut s'agir :\itemize{
\item d'un cercle de même effectif si \code{N} est fini et \code{R=Inf} ;
\item d'un cercle de rayon fixe si \code{N=Inf} et \code{R} est fini ;
\item d'un cercle mixte si si \code{N} et \code{R} sont finis.
}
Pour les \emph{cercles de même effectif}, \code{rings} sélectionne le plus petit cercle tel que le nombre d'observations valides dans ce cercle soit au moins égal à \code{N}.\cr
Pour les \emph{cercles de même rayon}, \code{rings} sélectionne toutes les grappes situées à une distance inférieure à \code{R} de la grappe centrale.\cr
Pour les \emph{cercles mixtes}, \code{rings} calcule tout d'abord le cercle d'effectif minimum et vérifie si son rayon est inférieur à \code{R}. Si oui, il conserve ce cercle, sinon il calcule le cercle de rayon maximum.

Il est possible de calculer différentes séries de cercles en transmettant plusieurs valeurs aux arguments \code{N} et \code{R}. \code{rings} calculera alors les cercles correspondant à chaque couple (N,R).
}
\value{
Renvoie \code{object} en complétant son élément \code{rings} pour chaque couple (N,R) calculé.

Chaque entrée de la liste \code{rings} est composé de trois éléments : \code{N}, effectif minimum des cercles ; \code{R}, rayon maximum des cercles et \code{estimates}, un tableau de données contenant les variables suivantes :\itemize{
\item "id" identifiant de la grappe.
\item "r.pos" nombre de cas positifs dans le cercle.
\item "r.n" nombre d'observations dans le cercle.
\item "r.prev" prévalence (en \%) observée dans le cercle.
\item "r.radius" rayon du cercle (en kilomètres si les coordonnées sont exprimées en degrés décimaux, dans l'unité de la projection sinon).
\item "r.clusters" nombre de grappes situées dans le cercle.
\item "r.wpos" (facultatif) somme des poids des cas positifs situés dans le cercle.
\item "r.wn" (facultatif) somme des poids des observations situées dans le cercle.
\item "r.wprev" (facultatif) prévalence pondérée (en \%) observée dans le cercle.
}
Les variables \emph{r.wpos}, \emph{r.wn} et \emph{r.wprev} ne sont calculées que si l'élément \code{clusters} de \code{object} contient des données pondérées.
}
\seealso{\code{\link{prevR-class}}.}

\references{
Larmarange J., Vallo R., Yaro S., Msellati P., Meda N., Ferry B. (2011) "Méthodes pour cartographier les tendances régionales de la prévalence du VIH à partir des Enquêtes Démographiques et de Santé (EDS)", \emph{Cybergeo: European Journal of Geography}, \url{http://cybergeo.revues.org/}.

Larmarange J. (2007) \emph{Prévalences du VIH en Afrique : validité d'une mesure}, thèse de doctorat en démographie, sous la direction de Benoît Ferry, université Paris Descartes, \url{http://tel.archives-ouvertes.fr/tel-00320283}.

Larmarange J., Vallo R., Yaro S., Msellati P., Meda N., Ferry B. (2006), "Cartographier les données des enquêtes démographiques et de santé à partir des coordonnées des zones d'enquête", \emph{Chaire Quételet, 29 novembre au 1er décembre 2006}, Université Catholique de Louvain, Louvain-la-Neuve, Belgique, \url{http://www.uclouvain.be/13881.html}.
}

\examples{
print(fdhs)
dhs <- rings(fdhs,N=c(100,200,300,400,500))
print(dhs)
}
\keyword{math}
\keyword{spatial}