\encoding{utf8}
\name{prevR.colors}
\alias{prevR.colors}
\alias{prevR.demo.pal}
\alias{prevR.colors.red}
\alias{prevR.colors.red.inverse}
\alias{prevR.colors.blue}
\alias{prevR.colors.blue.inverse}
\alias{prevR.colors.green}
\alias{prevR.colors.green.inverse}
\alias{prevR.colors.gray}
\alias{prevR.colors.gray.inverse}
\alias{prevR.colors.qgis.pal}
\title{Palettes de couleurs continues.}

\description{Fonctions générant des palettes de couleurs utilisables par les fonctions graphiques de \R, en particulier \code{\link[sp]{spplot}}. Elles créent des palettes de couleurs continues, les contrastes étant renforcés par l'éclaircissement ou l'assombrissement des valeurs extrêmes. \code{prevR.demo.pal} permet d'afficher les différentes palettes de couleurs disponibles. \code{prevR.colors.qgis.pal} permet d'exporter une palette dans un fichier texte lisible par le logiciel libre de cartographie Quantum GIS.}

\usage{
prevR.demo.pal(n, border, main, ch.col)
prevR.colors.red(n)
prevR.colors.red.inverse(n)
prevR.colors.blue(n)
prevR.colors.blue.inverse(n)
prevR.colors.green(n)
prevR.colors.green.inverse(n)
prevR.colors.gray(n)
prevR.colors.gray.inverse(n)
prevR.colors.qgis.pal(file, at, pal="red", inverse=FALSE)
}

\arguments{
\item{n}{nombre de couleurs devant constiter la palette.}
\item{border}{couleur de la bordure.}
\item{main}{titre du graphique.}
\item{ch.col}{liste des palettes à afficher.}
\item{file}{nom du fichier à créer avec son extension.}
\item{at}{liste des valeurs de la palette.}
\item{pal}{palette de couleur à utiliser ("red", "green", "blue" ou "gray").}
\item{inverse}{utiliser la palette inverse ?}
}
\details{
\code{\link{prevR.colors.red}} réalise un dégradé allant du blanc/jaune au rouge/rouge foncé.\cr
\code{\link{prevR.colors.blue}} réalise un dégradé allant du bleu pâle au bleu foncé.\cr
\code{\link{prevR.colors.green}} réalise un dégradé allant du vert pâle au vert foncé.\cr
\code{\link{prevR.colors.gray}} réalise un dégradé allant du blanc/gris clair au gris foncé/noir.\cr

Les fonctions avec le suffixe \emph{.inverse} réalisent les mêmes dégradés mais en partant des couleurs foncées vers les couleurs claires.
}

\value{
\code{\link{prevR.demo.pal}} affiche les différentes palettes.

\code{\link{prevR.colors.qgis.pal}} exporte la palette de couleurs dans un fichier texte utilisable par Quantum GIS.

Les autres fonctions renvoient une liste de couleurs codées de manière hexadécimale.
}

\note{
Pour obtenir la liste des couleurs au format RGB (Red/Green/Blue), utilisez la fonction \code{\link[grDevices]{col2rgb}}\{\pkg{grDevices}\}.

Le code de \code{\link{prevR.demo.pal}} a été repris sur celui de la fonction \code{demo.pal} décrite dans les exemples de la documentation de \code{\link[grDevices]{rainbow}}.
}

\seealso{D'autres palettes de couleurs existent sous \R. Voir \code{\link[grDevices]{rainbow}}\{\pkg{grDevices}\} ainsi que le package \pkg{RColorBrewer}.}

\examples{
prevR.demo.pal(25)
prevR.colors.red(5)
col2rgb(prevR.colors.red(5))

\dontrun{
prevR.colors.qgis.pal('palette.txt',seq(0,25,length.out=100),'red')
}
}
\keyword{color}