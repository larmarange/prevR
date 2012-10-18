\encoding{utf8}
\name{xyz2dataframe}
\alias{xyz2dataframe}
\title{Convert a surface in xyz to a data frame.}

\description{Several functions (for example \code{\link[GenKern]{KernSur}}\{\pkg{GenKern}\}) return a surface as a list "xyz" composed of three elements: vector of ordinates in the x dimension, vector of ordinates in the y dimension and a matrix with the values of the surface in x and y. This function transforms a list "xyz" into a data frame.}

\usage{
  xyz2dataframe(xyz, xcol = 1, ycol = 2, zcol = 3)
}

\arguments{
\item{xyz}{a list with 3 elements: a vector with x-coordinates, a vector with y-coordinates and and matrix with value for each point of coordinates x[i],y[j].}
\item{xcol}{x index.}
\item{ycol}{y index.}
\item{zcol}{z index.}
}

\note{\code{xyz} could be a list like \code{x,y,z1,z2,z3}. If so, \code{zcol} should be equal to \code{c("z1","z2","z3")} or \code{c(3,4,5)}.}

\value{A \code{data.frame}.}

\examples{
x <- c(2,4,6,8,10)
y <- x 
op <- KernSur(x,y, xgridsize=50, ygridsize=50,
              correlation=0, 
              xbandwidth=1, ybandwidth=1,
              range.x=c(0,13), range.y=c(0,13)
             )
str(op)

op.df <- xyz2dataframe(op)
str(op.df)
}
\keyword{manip}