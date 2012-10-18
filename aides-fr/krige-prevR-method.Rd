\encoding{utf8}
\name{krige,prevR-method}
\alias{krige,prevR-method}
\alias{krige-methods}
\alias{krige,ANY,prevR-method}
\alias{krige}
\alias{idw-methods}
\alias{idw,ANY,prevR-method}
\alias{idw,prevR-method}
\alias{idw}
\docType{methods}
\title{Interpolation spatiale (krigeage et distance inverse) pour les objets prevR.}

\description{Ces fonctions permettent d'interpoler spatialement les variables d'un élément \code{rings} d'un objet de classe \code{\link[=prevR-class]{prevR}}. La méthode \code{krige} effectue un krigeage ordinaire tandis que la méthode \code{idw} procède à une interpolation pondérée par l'inverse de la distance.}

\section{Methods}{\describe{\item{\code{signature(formula = "ANY", locations = "prevR")}}{}}}

\usage{
\S4method{krige}{ANY,prevR}(formula, locations,
      N = NULL, R = NULL, model = NULL,
      nb.cells = 100, cell.size = NULL,
      fit = "auto", keep.variance = FALSE, show.variogram = FALSE,
      \dots
     )
\S4method{idw}{ANY,prevR}(formula, locations,
    N = NULL, R = NULL,
    nb.cells = 100, cell.size = NULL, 
    idp = 2,
    \dots
   )
}

\arguments{
\item{formula}{variable à interpoler (voir détails).}
\item{locations}{objet de classe \code{\link[=prevR-class]{prevR}}.}
\item{N}{entier ou liste d'entiers indiquant les cercles à utiliser.}
\item{R}{entier ou liste d'entiers indiquant les cercles à utiliser.}
\item{model}{modèle de variogramme, défini par un appel à la fonction \code{\link[gstat]{vgm}}\{\pkg{gstat}\}.}
\item{nb.cells}{nombre de cellules sur la plus grande dimension (voir \code{\link[=as.SpatialGrid,prevR-method]{as.SpatialGrid}}).}
\item{cell.size}{côté d'une cellule (dans l'unité de la projection, voir \code{\link[=as.SpatialGrid,prevR-method]{as.SpatialGrid}}).}
\item{fit}{"auto" pour un ajustement automatique du semi-variogramme à partir des données, "manual" pour un ajustement graphique (inutilisé si \code{model} est fournir).}
\item{keep.variance}{renvoyer la variance des estimations ?}
\item{show.variogram}{afficher graphiquement le semi-variogramme utilisé ?}
\item{idp}{puissance de la pondération selon l'inverse de la distance (voir \code{\link[gstat]{idw}}\{\pkg{gstat}\}).}
\item{\dots}{arguments additionnels transmis à \code{\link[gstat]{krige}}\{\pkg{gstat}\} ou \code{\link[gstat]{idw}}\{\pkg{gstat}\}.}
}

\details{
\code{formula} permet de spécifier la ou les variables à interpoler. Il s'agit des variables disponibles dans l'élément \code{rings} de l'objet de classe \code{\link[=prevR-class]{prevR}} transmis. Les valeurs possibles sont "r.pos", "r.n", "r.prev", "r.radius", "r.clusters", "r.wpos", "r.wn" ou "r.wprev". Le nom des variables peut être indiqué sous forme d'une chaîne de caractères ou sous la forme de formules (exemple : \emph{list(r.pos~1,r.prev~1}). Seul les formules de la forme \emph{une.variable~1} sont acceptées. Pour des interpolations plus complexes, vous devez appeler manuellemet les fonctions \code{\link[gstat]{krige}} et \code{\link[gstat]{idw}} de \pkg{gstat}.

\code{N} et \code{R} permettent de sélectionner les cercles à utiliser pour récupérer la variable à interpoler. S'ils ne sont pas définis, la variable \code{formula} sera interpolée pour chacun des coupes (N,R) disponibles. Plusieurs interpolations peuvent être réalisées simultanément en transmettant une liste de plusieurs variables et/ou plusieurs valeurs de N et R.

Dans le cas d'un krigeage ordinaire, la méthode \code{krige} de \pkg{prevR} ajustera automatiquement un modèle (exponentiel) de semi-variogramme au semi-variogramme expérimental (\code{fit="auto"}). Si vous choisissez \code{fit="manual"}, la fonction affichera une fenêtre graphique (adaptée de \code{\link[geoR]{eyefit}}\{\pkg{geoR}\}) vous permettant d'ajuster visuellement le modèle de semi-variogramme au semi-variogramme expérimental. Vous pouvez également définir explicitement le modèle de semi-variogramme à utiliser avec le paramètre \code{model}.

Les interpolations sont effectuées sur une grille de points uniformément répartis, obtenue avec \code{\link[=as.SpatialGrid,prevR-method]{as.SpatialGrid}}.
}

\value{
Objet de classe \code{\link[sp:SpatialPixelsDataFrame-class]{SpatialPixelsDataFrame}}. Les surfaces estimées sont nommées à partir du nom de la variable interpolée, de N et de R (exemple : \emph{r.radius.N300.RInf}). Si la variance des estimations a été demandée à \code{krige} (\code{keep.variance=TRUE}), elle sera indiquée avec le suffixe \emph{.var}.

La valeur \code{NA} est appliquée aux points de la grille situés en-dehors de la zone d'étude\cr (voir \code{\link{NA.outside.SpatialPolygons}}).
}

\references{
Larmarange J., Vallo R., Yaro S., Msellati P., Meda N., Ferry B. (2011) "Méthodes pour cartographier les tendances régionales de la prévalence du VIH à partir des Enquêtes Démographiques et de Santé (EDS)", \emph{Cybergeo: European Journal of Geography}, \url{http://cybergeo.revues.org/}.
}

\note{
Les résultats peuvent être représentés graphiquement à l’aide de la fonction \code{\link[sp]{spplot}}\{\pkg{sp}\}.\cr \pkg{prevR} fournit plusieurs palettes de couleurs (voir \code{\link{prevR.colors}}) pouvant être utilisées avec \code{\link[sp]{spplot}}.

Les surfaces calculées peuvent être exportées avec la fonction \code{\link[maptools]{writeAsciiGrid}}\{\pkg{maptools}\}.
}
\seealso{\code{\link[gstat]{krige}}\{\pkg{gstat}\}, \code{\link[gstat]{idw}}\{\pkg{gstat}\}, \code{\link{rings,prevR-method}}.}

\examples{
dhs <- rings(fdhs,N=c(100,200,300,400,500))
radius.N300 <- krige('r.radius',dhs,N=300,nb.cells=200)
spplot(radius.N300,
       cuts=100, col.regions=prevR.colors.blue(101), 
       main="Radius of circle (N=300)"
      )

\dontrun{
prev.krige <- krige('r.wprev',dhs,N=c(100,300,500),R=Inf,
                    fit="manual",keep.variance=TRUE
                   )
str(prev.krige)
spplot(prev.krige,
       c('r.wprev.N100.RInf','r.wprev.N300.RInf','r.wprev.N500.RInf'),
       cuts=100, col.regions=prevR.colors.red(101)
      )
}
}
\keyword{smooth}
\keyword{spatial}