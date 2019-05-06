pkgname <- "recurse"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('recurse')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("calculateIntervalResidenceTime")
### * calculateIntervalResidenceTime

flush(stderr()); flush(stdout())

### Name: calculateIntervalResidenceTime
### Title: Calculates residence time within user-specified breaks
### Aliases: calculateIntervalResidenceTime

### ** Examples

data(martin)
revisits = getRecursions(martin, radius = 1)
breaks = strptime(c("2000-01-01 00:00:00", "2000-01-15 00:00:00", "2000-02-01 00:00:00"), 
format = "")
intervalResTime = calculateIntervalResidenceTime(revisits, breaks)




cleanEx()
nameEx("drawCircle")
### * drawCircle

flush(stderr()); flush(stdout())

### Name: drawCircle
### Title: Draws a circle
### Aliases: drawCircle

### ** Examples

data(martin)
revisits = getRecursions(martin, radius = 1)
plot(revisits, martin, legendPos = c(10, -15))
drawCircle(10, -10, 1)




cleanEx()
nameEx("getRecursions")
### * getRecursions

flush(stderr()); flush(stdout())

### Name: getRecursions
### Title: Calculates recursion information from the trajectory
### Aliases: getRecursions getRecursions.data.frame getRecursions.Move
###   getRecursions.MoveStack

### ** Examples

data(martin)
revisits = getRecursions(martin, radius = 1)
plot(revisits, martin, legendPos = c(10, -15))
drawCircle(10, -10, 1)




cleanEx()
nameEx("getRecursionsAtLocations")
### * getRecursionsAtLocations

flush(stderr()); flush(stdout())

### Name: getRecursionsAtLocations
### Title: Calculates recursion information from the trajectory for
###   specific locations
### Aliases: getRecursionsAtLocations getRecursionsAtLocations.data.frame
###   getRecursionsAtLocations.Move getRecursionsAtLocations.MoveStack

### ** Examples

data(martin)
locations = data.frame(x = c(-10, 0, 20), y = c(5, 0, 0))
revisits = getRecursionsAtLocations(martin, locations, radius = 1)
plot(revisits, locations, legendPos = c(10, -15), 
     alpha = 1, pch = 17, xlim = range(martin$x), ylim = range(martin$y))
points(martin$x, martin$y, pch = ".", col = "gray50")
drawCircle(10, -10, 1)




cleanEx()
nameEx("getRecursionsInPolygon")
### * getRecursionsInPolygon

flush(stderr()); flush(stdout())

### Name: getRecursionsInPolygon.Move
### Title: Calculates recursion information from the trajectory inside a
###   polygon
### Aliases: getRecursionsInPolygon.Move getRecursionsInPolygon
###   getRecursionsInPolygon.data.frame getRecursionsInPolygon.MoveStack

### ** Examples

data(track)
poly = sp::SpatialPolygons( list(
	 	sp::Polygons( list(sp::Polygon(cbind(c(4,6,6,3,4),c(1,2,4,3,1)))), ID = 1 )
	 	))
revisits = getRecursionsInPolygon(track, poly)



cleanEx()
nameEx("plot.recurse")
### * plot.recurse

flush(stderr()); flush(stdout())

### Name: plot.recurse
### Title: Calculates recursion information from the trajectory
### Aliases: plot.recurse

### ** Examples

data(martin)
revisits = getRecursions(martin, radius = 1)
plot(revisits, martin, legendPos = c(10, -15))
drawCircle(10, -10, 1)




### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
