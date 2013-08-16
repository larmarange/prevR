\encoding{utf8} 
\name{kde,prevR-method}
\alias{kde-methods}
\alias{kde,prevR-method}
\alias{kde}
\docType{methods}
\title{Estimateurs à noyau pour les objets prevR.}

\description{Cette fonction permet de calculer une surface de prévalences (ratio de deux surfaces d'intensité) et/ou une surface de risques relatifs (ratio de deux surfaces de densité) en utilisant des estimateurs à noyau gaussien à fenêtres adaptatives de même effectif et/ou de même rayon.}

\section{Methods}{\describe{\item{\code{signature(object = "prevR")}}{}}}

\usage{\S4method{kde}{prevR}(object,
    N = NULL, R = NULL, weighted = TRUE, 
    risk.ratio = FALSE, keep.details = FALSE,
    nb.cells = 100, cell.size = NULL,
    progression=TRUE
   )
}

\arguments{
\item{object}{objet de classe \code{\link[=prevR-class]{prevR}}.}
\item{N}{entier ou liste d'entiers indiquant les cercles à utiliser.}
\item{R}{entier ou liste d'entiers indiquant les cercles à utiliser.}
\item{weighted}{utiliser les données pondéréses (TRUE, FALSE ou "2") ?}
\item{risk.ratio}{calculer une surface de risques relatifs au lieu d'une surface de prévalences (TRUE, FALSE ou "2") ?} 
\item{keep.details}{renvoyer les surfaces des cas positifs et des cas observés (TRUE, FALSE) ?}
\item{nb.cells}{nombre de cellules sur la plus grande dimension (voir \code{\link[=as.SpatialGrid,prevR-method]{as.SpatialGrid}}).}
\item{cell.size}{côté d'une cellule (dans l'unité de la projection, voir \code{\link[=as.SpatialGrid,prevR-method]{as.SpatialGrid}}).}
\item{progression}{afficher une barre de progression des calculs ?}
}

\details{
Cette fonction calcule une surface des prévalences en faisant le ratio de la surface d'intensité (exprimée en cas par unité de surface) des cas positifs sur la surface d'intensité des cas observés. Elle peut également calculer une surface de risques relatifs qui correspod au ratio de la surface de densité (dont l'intégrale a été ramenée à l'unité) des cas positifs sur la surface de densité des cas observés.

La méthode utilisée ici est une variante de la technique des plus proches voisins. Il s'agit d'une estimation par noyau gaussien à fenêtres adaptatives, la taille des fenêtres étant déterminée par un effectif minimum d'observations dans le voisinage (voir \code{\link[=rings,prevR-method]{rings}} pour plus de détails). Il est également possible d'utiliser des fenêtres de taille fixe. Plus précisément, la fenêtre utilisée correspond à la moitié du rayon des cercles de même effectif et/ou de même rayon (paramètres \code{N} et \code{R}) calculés avec la fonction \code{\link[=rings,prevR-method]{rings}}.\cr
Voir les références pour une explication plus détaillée de la méthodologie implémentée.

\code{N} et \code{R} permettent de sélectionner les cercles à utiliser pour l'estimation des prévalences. S'ils ne sont pas définis, les surfaces de prévalences seront estimées pour chacun des couples (N,R) disponibles. Plusieurs interpolations peuvent être réalisées simultanément en transmettant une liste de plusieurs valeurs de N et R.
}

\value{Objet de classe \code{\link[sp:SpatialPixelsDataFrame-class]{SpatialPixelsDataFrame}}. Les surfaces estimées sont nommées à partir du nom de la variable interpolée, de N et de R (exemple : \emph{k.prev.N300.RInf}).

Les variables crées sont (selon les arguments passés à la fonction) :\itemize{
\item "k.pos" surface d'intensité non pondérée des cas positifs.
\item "k.obs" surface d'intensité non pondérée des cas observés.
\item "k.prev" surface non pondérée des prévalences (k.pos/k.obs).
\item "k.case" surface de densité non pondérée des cas positifs.
\item "k.control" surface de densité non pondérée des cas observés.
\item "k.rr" surface non pondérée des risques relatifs (k.case/k.control).
\item "k.wpos" surface d'intensité pondérée des cas positifs.
\item "k.wobs" surface d'intensité pondérée des cas observés.
\item "k.wprev" surface pondérée des prévalences (k.wpos/k.wobs).
\item "k.wcase" surface de densité pondérée des cas positifs.
\item "k.wcontrol" surface de densité pondérée des cas observés.
\item "k.wrr" surface pondérée des risques relatifs (k.wcase/k.wcontrol).
}
La valeur \code{NA} est appliquée aux points de la grille situés en-dehors de la zone d'étude\cr (voir \code{\link{NA.outside.SpatialPolygons}}).
}

\references{
Larmarange J., Vallo R., Yaro S., Msellati P., Meda N., Ferry B. (2011) "Méthodes pour cartographier les tendances régionales de la prévalence du VIH à partir des Enquêtes Démographiques et de Santé (EDS)", \emph{Cybergeo: European Journal of Geography}, \url{http://cybergeo.revues.org/}.
}

\note{
Les résultats peuvent être représentés graphiquement à l’aide de la fonction \code{\link[sp]{spplot}}\{\pkg{sp}\}.\cr \pkg{prevR} fournit plusieurs palettes de couleurs (voir \code{\link{prevR.demo.pal}}) pouvant être utilisées avec \code{\link[sp]{spplot}}.

Les surfaces calculées peuvent être exportées avec la fonction \code{\link[maptools]{writeAsciiGrid}}\{\pkg{maptools}\}.

Voir le package \pkg{\link[sparr:sparr-package]{sparr}} pour une autre approche méthodologique de calcul d'une surface de risques relatifs, adaptée à d'autres types de données que les Enquêtes Démographiques et de Santé (EDS).
}

\seealso{\code{\link[GenKern]{KernSur}}\{\pkg{GenKern}\}, \code{\link{rings,prevR-method}}.}

\examples{
dhs <- rings(fdhs,N=c(100,200,300,400,500))

prev.N300 <- kde(dhs,N=300,nb.cells=200)
spplot(prev.N300, 'k.wprev.N300.RInf',
       cuts=100, col.regions=prevR.colors.red(101),
       main="Regional trends of prevalence (N=300)"
      )

prev.krige <- kde(dhs,N=c(100,300,500),R=Inf,
                  nb.cells=200, risk.ratio=2, keep.details=FALSE
                 )
str(prev.krige)
dev.new()
spplot(prev.krige,
       c('k.wprev.N100.RInf','k.wprev.N300.RInf','k.wprev.N500.RInf'),
       cuts=100, col.regions=prevR.colors.red(101)
      )
}
\keyword{smooth}
\keyword{spatial}