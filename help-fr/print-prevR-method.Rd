\encoding{utf8}
\name{print,prevR-method}
\docType{methods}
\alias{print-methods}
\alias{print,ANY-method}
\alias{print,prevR-method}
\alias{print}
\title{Affiche le résumé d'un objet prevR.}

\description{Méthode \code{print} pour les objets de classe \code{\link[=prevR-class]{prevR}} : affiche un résumé des caractéristiques de l'objet.}

\section{Methods}{\describe{\item{\code{signature(x = "prevR")}}{}}}

\usage{\S4method{print}{prevR}(x)}

\arguments{
\item{x}{objet de classe \code{\link[=prevR-class]{prevR}}.}
}

\note{Identique à \code{\link{show,prevR-method}}.}

\seealso{\code{\link{summary,prevR-method}}.}

\examples{
print(fdhs)
dhs <- rings(fdhs,N=c(100,300,500))
print(dhs)
}