\encoding{utf8}
\name{prevR.colors}
\alias{prevR.colors}
\alias{prevR.demo.pal}
\alias{prevR.colors.red}
\alias{prevR.colors.red.inverse}
\alias{prevR.colors.blue}
\alias{prevR.colors.blue.inverse}
\alias{prevR.colors.green}
\alias{prevR.colors.green.inverse}
\alias{prevR.colors.gray}
\alias{prevR.colors.gray.inverse}
\alias{prevR.colors.qgis.pal}
\title{Continuous color palettes.}

\description{Functions generating color palettes useable with \R graphical functions, in particular with \code{\link[sp]{spplot}}. These palettes are continuous, contrast being accentuated by darkening and lightening extrem values. \code{prevR.demo.pal} plot the available palettes. \code{prevR.colors.qgis.pal} export a palette in a text file readable by Quantum GIS, an open-source mapping software.}

\usage{
prevR.demo.pal(n, border, main, ch.col)
prevR.colors.red(n)
prevR.colors.red.inverse(n)
prevR.colors.blue(n)
prevR.colors.blue.inverse(n)
prevR.colors.green(n)
prevR.colors.green.inverse(n)
prevR.colors.gray(n)
prevR.colors.gray.inverse(n)
prevR.colors.qgis.pal(file, at, pal="red", inverse=FALSE)
}

\arguments{
\item{n}{number of different colors in the palette.}
\item{border}{border color.}
\item{main}{title.}
\item{ch.col}{list of palettes to show.}
\item{file}{file name with extension.}
\item{at}{list of values of the palette.}
\item{pal}{color palette to use ("red", "green", "blue" or "gray").}
\item{inverse}{use the inverse palette?}
}
\details{
\code{\link{prevR.colors.red}} produces a color gradation from white/yellow to red/dark red.\cr
\code{\link{prevR.colors.blue}} produces a color gradation from light blue to dark blue.\cr
\code{\link{prevR.colors.green}} produces a color gradation from light green to dark green.\cr
\code{\link{prevR.colors.gray}} produces a color gradation from white/light gray to dark gray/black.\cr

Functions with a suffix \emph{.inverse} produce the same color gradation, but from dark colors to light ones.
}

\value{
\code{\link{prevR.demo.pal}} plot the color palettes.

\code{\link{prevR.colors.qgis.pal}} export a color palette in a texte file readable by Quantum GIS.

The other functions return a list of colors coded in hexadecimal.
}

\note{
To obtain the liste of colors in RGB (Red/Green/Blue), use the function \code{\link[grDevices]{col2rgb}}\{\pkg{grDevices}\}.

The code of \code{\link{prevR.demo.pal}} was adapted from the function \code{demo.pal} presented in the examples of \code{\link[grDevices]{rainbow}}.
}

\seealso{Other color palettes are available in  \R. See for example \code{\link[grDevices]{rainbow}}\{\pkg{grDevices}\} or the package \pkg{RColorBrewer}.}

\examples{
prevR.demo.pal(25)
prevR.colors.red(5)
col2rgb(prevR.colors.red(5))

\dontrun{
prevR.colors.qgis.pal('palette.txt', seq(0,25,length.out=100), 'red')
}
}
\keyword{color}