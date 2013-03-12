\encoding{utf8}
\name{show,prevR-method}
\alias{show}
\alias{show-methods}
\alias{show,prevR-method}
\title{Summary of a prevR object.}

\description{Method \code{show} for objects of class \code{\link[=prevR-class]{prevR}}: shows a summary of the object's characteristics .}

\section{Methods}{\describe{\item{\code{signature(object = "prevR")}}{}}}

\usage{\S4method{show}{prevR}(object)}
\arguments{
\item{object}{object of class \code{\link[=prevR-class]{prevR}}.}
}

\note{
Exactly the same as \code{\link{print,prevR-method}}.
}

\seealso{\code{\link{summary,prevR-method}}.}

\examples{
fdhs
\dontrun{
	dhs <- rings(fdhs,N=c(100,300,500))
	dhs
}
}