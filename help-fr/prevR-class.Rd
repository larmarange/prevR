\encoding{utf8}
\name{prevR-class}
\Rdversion{1.1}
\docType{class}
\alias{prevR-class}

\title{Objets de classe prevR.}

\description{Classe utilisée par le package \pkg{prevR}}

\section{Objects from the Class}{Les objets de cette classe peuvent être créés à l'aide de la fonction \code{\link{as.prevR}}.}

\section{Slots}{\describe{
\item{\code{clusters}}{objet \code{data.frame} contenant les données observées avec une ligne par grappe. Les colonnes sont nommées :\itemize{
  \item "id" identifiant de la grappe.
  \item "x" longitude.
  \item "y" latitude.
  \item "n" nombre d'observations valides dans la grappe.
  \item "pos" nombre de cas positifs observés dans la grappe.
  \item "prev" prévalence (en \%) observée dans la grappe.
  \item "wn" (facultatif) somme des poids des observations de la grappe.
  \item "wpos" (facultatif) somme des poids des cas positifs de la grappe.
  \item "wprev" (facultatif) prévalence pondérée (en \%) observée dans la grappe.
  \item "c.type" (facultatif) variable de classification des grappes.
}}
\item{\code{boundary}}{objet de classe \code{\link[sp:SpatialPolygons-class]{SpatialPolygons}} définissant les limites de la zone d'étude.}
\item{\code{proj}}{objet de classe \code{\link[sp:CRS-class]{CRS}} définissant le système de coordonnées utilisé.}
\item{\code{rings}}{liste contenant les résultats de la fonction \code{\link[=rings,prevR-method]{rings}}. Chaque entrée de la liste est composée de trois éléments : \code{N}, effectif minimum des cercles ; \code{R}, rayon maximum des cercles et \code{estimates}, un tableau de données contenant les variables suivantes :\itemize{
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
Remarque : la liste \code{rings} est nommée, le nom de chacun de ses éléments étant de la forme N(valeur de N).R(valeur de R), par exemple \emph{N300.RInf}.
}
}}

\section{Methods}{\describe{
\item{as.data.frame}{\code{signature(x = "prevR")} transforme un objet prevR en tableau de données.}
\item{as.SpatialGrid}{\code{signature(object = "prevR")} calcule une grille de points.}
\item{export}{\code{signature(object = "prevR")} exporte un objet prevR en shapefile, fichier dbf ou fichier texte.}
\item{idw}{\code{signature(formula = "ANY", locations = "prevR")} effectue une interpolation spatiale selon une pondération inverse de la distance.}
\item{kde}{\code{signature(object = "prevR")} estime une surface de prévalence par des estimateurs à noyaux}
\item{krige}{\code{signature(formula = "ANY", locations = "prevR")} effecture une interpolatio spatiale par krigeage ordinaire.}
\item{plot}{\code{signature(x = "prevR", y = "ANY")} représente graphiquement les données d'un objet prevR.}
\item{print}{\code{signature(x = "prevR")} affiche le résumé d'un objet prevR.}
\item{rings}{\code{signature(object = "prevR")} calcule des cercles de même effectif et/ou de même rayon.}
\item{show}{\code{signature(object = "prevR")} affiche le résumé d'un objet prevR.}
\item{summary}{\code{signature(object = "prevR")} affiche un résumé des variables d'un objet prevR.}
\item{changeproj}{\code{signature(object = "prevR")} modifie le système de coordonnées d'un objet prevR.}
}}

\seealso{  
  \code{\link{as.prevR}}, \code{\link{is.prevR}}, \code{\link{changeproj,prevR-method}}, \code{\link{rings,prevR-method}}, \code{\link{print,prevR-method}}, \code{\link{plot,prevR-method}}, \code{\link{summary,prevR-method}}, \code{\link{kde,prevR-method}}, \code{\link{krige,prevR-method}}, \code{\link{idw,prevR-method}}, \code{\link{export,prevR-method}}.
}

\examples{
showClass("prevR")

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

dhs <- rings(fdhs,N=c(100,300,500))
str(dhs)
print(dhs)
}
\keyword{classes}