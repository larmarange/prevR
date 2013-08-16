\encoding{utf8}
\name{summary,prevR-method}
\docType{methods}
\alias{summary-methods}
\alias{summary,prevR-method}
\alias{summary}
\title{Affiche un résumé des variables contenues dans un objet prevR.}

\description{Méthode \code{summary} pour les objets de classe \code{\link[=prevR-class]{prevR}} : affiche un résumé des variables de l'objet.}

\section{Methods}{\describe{\item{\code{signature(object = "prevR")}}{}}}

\usage{\S4method{summary}{prevR}(object,probs = c(0,.10,.25,.50,.75,.80,.90,.95,.99,1))}

\arguments{
\item{object}{objet de classe \code{\link[=prevR-class]{prevR}}.}
\item{probs}{vecteur de probabilités pour le calcul des quantiles des rayons des cercles.}
}

\seealso{\code{\link{print,prevR-method}}.}

\examples{
summary(fdhs)
dhs <- rings(fdhs,N=c(100,300,500))
summary(dhs)
}