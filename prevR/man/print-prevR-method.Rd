\encoding{utf8}
\name{print,prevR-method}
\docType{methods}
\alias{print-methods}
\alias{print,ANY-method}
\alias{print,prevR-method}
\alias{print}
\title{Summary of a prevR object.}

\description{Method \code{print} for objects of class \code{\link[=prevR-class]{prevR}}: shows a summary of the object's characteristics .}

\section{Methods}{\describe{\item{\code{signature(x = "prevR")}}{}}}

\usage{\S4method{print}{prevR}(x)}

\arguments{
\item{x}{object of class \code{\link[=prevR-class]{prevR}}.}
}

\note{Exactly the same as\code{\link{show,prevR-method}}.}

\seealso{\code{\link{summary,prevR-method}}.}

\examples{
print(fdhs)
\dontrun{
	dhs <- rings(fdhs,N=c(100,300,500))
	print(dhs)
}
}