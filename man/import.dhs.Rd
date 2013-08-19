\encoding{utf8}
\name{import.dhs}
\alias{import.dhs}
\title{Import DHS data.}

\description{This step by step function guides users to import data from a Demographic and Health Survey (DHS) and create an object of class \code{\link[=prevR-class]{prevR}}.}

\usage{import.dhs(file.sav, file.dbf)}

\arguments{
\item{file.sav}{DHS data (one individual per line) in SPSS format (.sav), downloaded from \url{http://www.measuredhs.com}.}
\item{file.dbf}{GPS position of clusters in DATABASE format (.dbf), downloaded from \url{http://www.measuredhs.com}.}
}

\note{
If you don't provide the precise path of files, \R will check the working directory (see \code{\link[base]{setwd}}). To specify the file path, see \code{\link[base]{file.path}}.

This function was developed specifically for importing DHS. For a generic function for creating an object of class \code{\link[=prevR-class]{prevR}}, see \code{\link{as.prevR}}.

The package \pkg{foreign} is required to use this function.
}

\seealso{\code{\link{as.prevR}}, \code{\link{prevR-class}}.}

\examples{
\dontrun{
import.dhs("data.sav", "gps.dbf")
}
}
\keyword{manip}