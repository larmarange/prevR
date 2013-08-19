# Transforme une liste de 3 elements (x, y et z) comme le resultat de KernSur
# en un data.frame
# note : z peut etre une liste de matrices
xyz2dataframe <- function(xyz, xcol=1, ycol=2, zcol=3) {
    nx <- length(xyz[[xcol]])
    ny <- length(xyz[[ycol]])
    for (i in 1:ny) {
        z <- xyz[zcol]
        for (k in 1:length(zcol)) z[[k]] <- z[[k]][,i]
        y <- rep(xyz[[ycol]][i],nx)
        x <- xyz[xcol]
        temp <- data.frame(x,y,z)
        names(temp) <- c(names(xyz[xcol]),names(xyz[ycol]),names(xyz[zcol]))
        if (i==1)
            res <- temp
        else
            res <- rbind(res,temp)
    }
    return(res)
}