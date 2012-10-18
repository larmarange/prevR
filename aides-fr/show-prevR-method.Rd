\encoding{utf8}
\name{show,prevR-method}
\alias{show}
\alias{show-methods}
\alias{show,prevR-method}
\title{Affiche le résumé d'un objet prevR.}

\description{Méthode \code{show} pour les objets de classe \code{\link[=prevR-class]{prevR}} : affiche un résumé des caractéristiques de l'objet.}

\section{Methods}{\describe{\item{\code{signature(object = "prevR")}}{}}}

\usage{\S4method{show}{prevR}(object)}
\arguments{
\item{object}{objet de classe \code{\link[=prevR-class]{prevR}}.}
}

\note{
Identique à \code{\link{print,prevR-method}}.
}

\seealso{\code{\link{summary,prevR-method}}.}

\examples{
fdhs
dhs <- rings(fdhs,N=c(100,300,500))
dhs
}