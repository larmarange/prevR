\encoding{utf8}
\name{summary,prevR-method}
\docType{methods}
\alias{summary-methods}
\alias{summary,prevR-method}
\alias{summary}
\title{Detailed summary of the variables of a prevR object}

\description{Method \code{summary} for objects of class \code{\link[=prevR-class]{prevR}}: shows a summary of the variables of the object.}

\section{Methods}{\describe{\item{\code{signature(object = "prevR")}}{}}}

\usage{\S4method{summary}{prevR}(object, probs = c(0,.10,.25,.50,.75,.80,.90,.95,.99,1))}

\arguments{
\item{object}{object of class \code{\link[=prevR-class]{prevR}}.}
\item{probs}{vector of probabilities with values in [0,1] for calculating quantiles of the rings radii (see examples).}
}

\seealso{\code{\link{print,prevR-method}}.}

\examples{
summary(fdhs)
dhs <- rings(fdhs, N=c(100,300,500))
summary(dhs)
summary(dhs, c(0, 0.25, 0.5, 0.75, 1))
}