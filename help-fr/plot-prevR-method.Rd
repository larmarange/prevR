\encoding{utf8}
\name{plot,prevR-method}
\alias{plot}
\alias{plot-methods}
\alias{plot,prevR-method}
\alias{plot,prevR,missing-method}
\docType{methods}
\title{Graphiques pour les objets prevR.}

\description{Méthode \code{plot} pour les objets de classe \code{\link[=prevR-class]{prevR}}. Affiche la position des grappes, le nombre d'observations ou le nombre de cas positifs.}

\section{Methods}{\describe{\item{\code{signature(x = "prevR", y = "missing")}}{}}}

\usage{
\S4method{plot}{prevR,missing}(x,
     type = "position",
     add.legend = TRUE, 
     legend.location = "bottomright",
     factor.size = 0.2,
     new.window = TRUE, 
     axes = F,
     \dots
    )
}
\arguments{
\item{x}{objet de classe \code{\link[=prevR-class]{prevR}}.}
\item{type}{graphique à afficher :\itemize{
\item "position" position des grappes.
\item "c.type" grappes selon \code{c.type}.
\item "count" nombre d'observations par grappe.
\item "flower" nombre de cas positifs par grappe.}
}
\item{add.legend}{ajouter une légende ?}
\item{legend.location}{position de la légende.}
\item{factor.size}{facteur d'agrandissement/réduction des cercles (pour \code{type="count"}).}
\item{new.window}{afficher dans une nouvelle fenêtre ?} 
\item{axes}{afficher les axes ?}
\item{...}{arguments additionnels transmis à \code{\link[graphics]{title}}.}
}

\details{
L'argument \code{legend.location} peut recevoir les valeurs suivantes : \emph{"bottomright"}, \emph{"bottom"}, \emph{"bottomleft"}, \emph{"left"}, \emph{"topleft"}, \emph{"top"}, \emph{"topright"}, \emph{"right"} et \emph{"center"}.

Utilisez, entre autres, l'argument \code{main} pour définir un titre et \code{sub} pour un sous-titre.
}
\seealso{\code{\link[graphics]{title}}\{\pkg{graphics}\}, \code{\link[graphics]{legend}}\{\pkg{graphics}\}.}

\examples{
\dontshow{par(ask = TRUE)}
plot(fdhs,type = "position",main="position",new.window=FALSE,axes=TRUE)
plot(fdhs,type = "c.type",main="c.type",new.window=FALSE)
plot(fdhs,type = "count",main="count",factor.size = 0.1,new.window=FALSE)
plot(fdhs,type = "flower",main="flower",new.window=FALSE)
\dontshow{par(ask = FALSE)}
}
\keyword{hplot}