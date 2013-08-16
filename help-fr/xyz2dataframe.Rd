\encoding{utf8}
\name{xyz2dataframe}
\alias{xyz2dataframe}
\title{Transforme une surface définie en xyz en un tableau de données.}

\description{Plusieurs fonctions (par exemple \code{\link[GenKern]{KernSur}}\{\pkg{GenKern}\}) gèrent les surfaces sous la forme d'une liste, dite xyz, de trois éléments : un vecteur avec les coordonnées en x, un vecteur avec les coordonnées en y et une matrice avec les valeurs de la surface en x et y.   La présente fonction transforme une liste xyz en un tableau de données.}

\usage{
  xyz2dataframe(xyz, xcol = 1, ycol = 2, zcol = 3)
}

\arguments{
\item{xyz}{une liste contenant 3 éléments : un élément vecteur contenant les coordonnees sur x, un élément vecteur contenant les coordonnees sur y et un élément matrice contenant des valeurs en chaque point de coordonnées x[i],y[j].}
\item{xcol}{indice de x.}
\item{ycol}{indice de y.}
\item{zcol}{indice de z.}
}

\note{\code{xyz} peut être une liste de la forme \code{x,y,z1,z2,z3}. L'argument \code{zcol} sera alors égal à \code{c("z1","z2","z3")} ou \code{c(3,4,5)}.}

\value{Un \code{data.frame}.}

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