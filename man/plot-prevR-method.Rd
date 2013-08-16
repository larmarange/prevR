\encoding{utf8}
\name{plot,prevR-method}
\alias{plot}
\alias{plot-methods}
\alias{plot,prevR-method}
\alias{plot,prevR,missing-method}
\docType{methods}
\title{Plot object of class prevR.}

\description{Method \code{plot} for object of class \code{\link[=prevR-class]{prevR}}. Plot clusters, number of observations per cluster or number of positive cases per cluster.}

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
\item{x}{object of class \code{\link[=prevR-class]{prevR}}.}
\item{type}{graph to plot:\itemize{
\item "position" clusters position.
\item "c.type" clusters per \code{c.type}.
\item "count" number of observatios per cluster.
\item "flower" number of positive cases per cluster.}
}
\item{add.legend}{add a legend?}
\item{legend.location}{legend location.}
\item{factor.size}{scale factor of rings (for \code{type="count"}).}
\item{new.window}{plot in a new window?} 
\item{axes}{show axes?}
\item{...}{additional arguments transmitted to \code{\link[graphics]{title}}.}
}

\details{
Available values for \code{legend.location} are: \emph{"bottomright"}, \emph{"bottom"}, \emph{"bottomleft"}, \emph{"left"}, \emph{"topleft"}, \emph{"top"}, \emph{"topright"}, \emph{"right"} use \emph{"center"}.

Use \code{main} to define a title and \code{sub} for a subtitle (see \code{\link[graphics]{title}}\{\pkg{graphics}\}).
}
\seealso{\code{\link[graphics]{title}}\{\pkg{graphics}\}, \code{\link[graphics]{legend}}\{\pkg{graphics}\}.}

\examples{
\dontshow{par(ask = TRUE)}
plot(fdhs, type = "position", main="position", new.window=FALSE, axes=TRUE)
plot(fdhs, type = "c.type", main="c.type", new.window=FALSE)
plot(fdhs, type = "count", main="count", factor.size = 0.1, new.window=FALSE)
plot(fdhs, type = "flower", main="flower", new.window=FALSE)
\dontshow{par(ask = FALSE)}
}
\keyword{hplot}