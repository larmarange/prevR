\encoding{utf8}
\name{as.prevR}
\alias{as.prevR}
\title{Création d'objet prevR.}
\description{Cette fonction permet de créer un objet de la classe \code{\link[=prevR-class]{prevR}} à partir d'un tableau de données.}
\usage{as.prevR(data, col, boundary = NULL, proj = "+proj=longlat")}
\arguments{
\item{data}{un tableau de données, chaque ligne correspondant à une grappe observée.}
\item{col}{un vecteur permettant d'identifier les variables de \code{data} à utiliser pour construire l'élément \code{clusters} de l'objet \code{\link[=prevR-class]{prevR}}.\cr
Les noms des colonnes de \code{clusters} sont normalisés et ont pour valeur :\itemize{
\item "id" identifiant de la grappe.
\item "x" longitude de la grappe.
\item "y" latitude de la grappe.
\item "n" nombre d'observations.
\item "pos" nombre des cas positifs.
\item "wn" (facultatif) somme des poids individuels des observations.
\item "wpos" (facultatif) somme des poids individuels des cas positifs.
\item "c.type" (facultatif) variable de classification des grappes (utilisée seulement par \code{\link[=show,prevR-method]{plot}}).
}
Voir les exemples.
}
\item{boundary}{objet de classe \code{\link[sp:SpatialPolygons-class]{SpatialPolygons}} définissant les limites de la zone d'étude.}
\item{proj}{projection dans laquelle les coordonnées des grappes sont exprimées dans \code{data} (longitude / latitude par défaut).}
}

\details{
Seuls "x", "y" "n" et "pos" sont obligatoires dans \code{col}. Si "id" n'est pas spécifié, un identifiant numérique sera créé.

\code{proj} définie la projection utilisée par \code{data}. Il peut s'agir d'une chaîne de caractères correspondant à une projection \emph{PROJ.4} (voir \url{http://trac.osgeo.org/proj/} pour plus de détails) ou un objet de classe \code{\link[sp:CRS-class]{CRS}}\{\pkg{sp}\}.\cr
Si la projection de \code{boundary} est précisée dans son élément \code{proj4string}, \code{boundary} sera transformée dans la projection définie par \code{proj}. Si l'élément \code{proj4string} est manquant, \code{boundary} sera considéré comme étant défini dans le système de coordonnées de \code{proj}.

Si \code{boundary} n'est pas précisé (\code{NULL}), un rectangle défini par les coordonnées maximales et minimales de \code{data} sera calculé.\cr
\code{boundary} peut être le résultat d'un appel à la fonction \code{\link{create.boundary}}.

\code{as.prevR} ne permet pas de changer de projection. Pour cela, utilisez \code{\link[=changeproj,prevR-method]{changeproj}} sur le résultat de \code{as.prevR}.\cr
}

\value{Un objet de classe \code{\link[=prevR-class]{prevR}} (voir la documentation de la classe pour plus de détails).}

\seealso{\code{\link{prevR-class}}, \code{\link{create.boundary}}, \code{\link[=changeproj,prevR-method]{changeproj}}, \code{\link{import.dhs}}.}
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

str(dhs)
print(dhs)
}
\keyword{manip}