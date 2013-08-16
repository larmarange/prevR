\encoding{utf8}
\name{prevR-package}
\alias{prevR-package}
\alias{prevR}
\docType{package}
\title{Estimation des tendances régionales d'une prévalence à partir d'une EDS.}
\description{\pkg{prevR} fournit des fonctions pour l'estimation spatiale d'une surface de prévalences ou d'une surface de risques relatifs à partir de données issues d'une Enquête Démographique et de Santé (EDS) ou d'une enquête analogue.}
\details{\tabular{ll}{
Package : \tab prevR\cr
Type : \tab Package\cr
Version : \tab 2.0\cr
Licence : \tab CeCILL-C - \url{http://cecill.info/licences/Licence_CeCILL-C_V1-fr.html}\cr
Site web : \tab \url{http://www.ceped.org/prevR},\cr
 \tab \url{http://joseph.larmarange.net/prevR}\cr
Dépendances : \tab \pkg{\link[methods:methods-package]{methods}}, \pkg{sp}, \pkg{gstat}, \pkg{maptools}, \pkg{\link[fields:fields-package]{fields}}, \pkg{GenKern}, \pkg{rgdal}, \pkg{geoR}, \pkg{\link[tcltk:tcltk-package]{tcltk}}\cr
}

Ce package propose une approche méthodologique pour estimer les tendances régionales d’une prévalence à partir d’enquêtes auprès des ménages échantillonnées en grappes à deux degrés (comme les Enquêtes Démographiques et de Santé). Dans de telles enquêtes, les cas positifs et les cas observés sont positionnés spatialement au niveau de la grappe enquêtée.

Ce package fournit des fonctions pour estimer une surface des prévalences à partir d’estimateurs à noyau à fenêtres adaptatives de même effectif (variante de la méthode des plus proches voisins) ou de même rayon. La surface des prévalences peut également être calculée en procédant à une interpolation spatiale (krigeage ou pondération selon l’inverse de la distance) suite à un lissage préalable des données utilisant des moyennes mobiles basées sur des cercles de même effectif ou de même rayon.

L’approche par les estimateurs à noyau permet également d’estimer une surface de risques relatifs.

Pour une démonstration du package, utilisez la commande \code{demo(prevR)}.

Le contenu de \pkg{prevR} peut être subdivisé ainsi :

\emph{Données}\cr
\code{\link{fdhs}} est un ensemble de données fictives permettant de tester le package.\cr
\code{\link{TMWorldBorders}} fournit les frontières de chaque pays du monde et peut-être utilisé pour définir les limites de la zone d’étude.

\emph{Création d’objet}\cr
Les fonctions de \pkg{prevR} manipulent des objets de classe \code{\link[=prevR-class]{prevR}}.\cr
\code{\link{import.dhs}} permet d’importer facilement les données d'une EDS (Enquête Démographique et de Santé), telles que téléchargées sur le site \url{http://www.measuredhs.com}, en accompagnant l’utilisateur pas à pas.\cr
\code{\link{as.prevR}} est une fonction générique pour créer un objet \code{\link[=prevR-class]{prevR}}.\cr
\code{\link{create.boundary}} peut être utilisée pour sélectionner les frontières d’un pays et les fournir à \code{\link{as.prevR}} afin de définir la zone d’étude.

\emph{Visualisation des données}\cr
Les méthodes \code{\link[=show,prevR-method]{show}}, \code{\link[=print,prevR-method]{print}} et \code{\link[=summary,prevR-method]{summary}} permettent de visualiser le contenu d’un objet \code{\link[=prevR-class]{prevR}}.\cr
La méthode \code{\link[=plot,prevR-method]{plot}} peut être utilisée sur les objets \code{\link[=prevR-class]{prevR}} pour visualiser la zone d’étude, la position des grappes enquêtées, le nombre d’observations ou le nombre de cas positifs par grappe.

\emph{Manipulation des données}\cr
La méthode \code{\link[=changeproj,prevR-method]{changeproj}} modifie le système de projection des coordonnées.\cr
La méthode \code{\link[=as.data.frame.prevR]{as.data.frame}} transforme un objet \code{\link[=prevR-class]{prevR}} en un tableau de données.\cr
La méthode \code{\link[=export,prevR-method]{export}} exporte les données et/ou la zone d’étude au format texte, dbf ou shapefile.

\emph{Analyses}\cr
\code{\link[=rings,prevR-method]{rings}} calcule les cercles de même effectif et/ou de même rayon.\cr
\code{\link[=kde,prevR-method]{kde}} calcule une surface de prévalence ou une surface de risques relatifs à partir d’estimateurs à noyau gaussien à fenêtres adaptatives.\cr
\code{\link[=krige,prevR-method]{krige}} effectue une interpolation spatiale par krigeage ordinaire.\cr
\code{\link[=idw,prevR-method]{idw}} calcule une interpolation spatiale en utilisant une pondération selon l’inverse de la distance.

\emph{Visualisation et exportation des résultats}\cr
\code{\link[=kde,prevR-method]{kde}}, \code{\link[=krige,prevR-method]{krige}} et \code{\link[=idw,prevR-method]{idw}} renvoient des objets de classe \code{\link[sp:SpatialPixelsDataFrame-class]{SpatialPixelsDataFrame}}\{\pkg{sp}\}.\cr
Ces derniers peuvent être représentés graphiquement à l’aide de la fonction \code{\link[sp]{spplot}}\{\pkg{sp}\}.\cr \pkg{prevR} fournit plusieurs palettes de couleurs (voir \code{\link{prevR.colors}}) pouvant être utilisée avec \code{\link[sp]{spplot}}.\cr
Les surfaces calculées peuvent être exportées avec la fonction \code{\link[maptools]{writeAsciiGrid}}\{\pkg{maptools}\}.
}

\author{
Joseph Larmarange \email{joseph.larmarange@ird.fr}\cr
CEPED (Université Paris Descartes Ined IRD) - IRD
}

\section{Acknowledgement}{
\pkg{prevR} a été développé avec l'appui financier de l'Agence Nationale de Recherches sur le Sida et les hépatites virales (ANRS - \url{http://www.anrs.fr}) et de l'Institut de Recherche pour le Développement (IRD - \url{http://www.ird.fr}), et le soutien technique de la SARL LYSIS (info@lysis-consultants.fr).
}

\section{Citation}{
Pour citer \pkg{prevR}, l’utilisateur pourra se référer à l’article suivant, en cours de publication :\cr
Larmarange J., Vallo R., Yaro S., Msellati P., Meda N., Ferry B. (2011) "Méthodes pour cartographier les tendances régionales de la prévalence du VIH à partir des Enquêtes Démographiques et de Santé (EDS)", \emph{Cybergeo: European Journal of Geography}, \url{http://cybergeo.revues.org/}.
}

\references{
Larmarange J., Vallo R., Yaro S., Msellati P., Meda N., Ferry B. (2011) "Méthodes pour cartographier les tendances régionales de la prévalence du VIH à partir des Enquêtes Démographiques et de Santé (EDS)", \emph{Cybergeo: European Journal of Geography}, \url{http://cybergeo.revues.org/}.

Larmarange J. (2007) \emph{Prévalences du VIH en Afrique : validité d'une mesure}, thèse de doctorat en démographie, sous la direction de Benoît Ferry, université Paris Descartes, \url{http://tel.archives-ouvertes.fr/tel-00320283}.

Larmarange J., Vallo R., Yaro S., Msellati P., Meda N., Ferry B. (2006), "Cartographier les données des enquêtes démographiques et de santé à partir des coordonnées des zones d'enquête", \emph{Chaire Quételet, 29 novembre au 1er décembre 2006}, Université Catholique de Louvain, Louvain-la-Neuve, Belgique, \url{http://www.uclouvain.be/13881.html}.
}

\examples{
\dontshow{par(ask = TRUE)}
# Creation d'un objet de classe prevR
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

plot(dhs, main="Clusters position",new.window=FALSE)
plot(dhs, type="c.type", main="Clusters by residence",new.window=FALSE)
plot(dhs, type="count", main="Observations by cluster",new.window=FALSE)
plot(dhs, type="c.type", main="Positive cases by cluster",new.window=FALSE)


# Changement de systeme de coordonnees
plot(dhs,axes=TRUE,new.window=FALSE)
dhs <- changeproj(dhs,
       "+proj=utm +zone=30 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
print(dhs)
plot(dhs,axes=TRUE,new.window=FALSE)

# Calcul des cercles de meme effectif pour differentes valeurs de N
dhs <- rings(fdhs,N=c(100,200,300,400,500))
print(dhs)
summary(dhs)

# Surface des prevalence pour N=300
prev.N300 <- kde(dhs,N=300,nb.cells=200)
spplot(prev.N300, 'k.wprev.N300.RInf',
       cuts=100, col.regions=prevR.colors.red(101),
       main="Regional trends of prevalence (N=300)"
      )

# Surface des rayons des cercles de meme effectif
radius.N300 <- krige('r.radius',dhs,N=300,nb.cells=200)
spplot(radius.N300, 
       cuts=100, col.regions=prevR.colors.blue(101),
       main="Radius of circle (N=300)"
      )
\dontshow{par(ask = FALSE)}
}
\keyword{package}