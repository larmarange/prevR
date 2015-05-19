#'  Quick prevR analysis and plot
#'  
#'  This function performs several analysis in one go: (i) apply \code{\link[dest=rings,prevR-method]{rings}}; 
#'  (ii) compute prevalence surface with \code{\link[dest=kde,prevR-method]{kde}}; (iii) compute the surface
#'  of rings radii with \code{\link[dest=krige,ANY,prevR-method]{krige}}; (iv) plot prevalence surface using
#'  \code{\link{prevR.colors.red}} and add rings radii as a contour plot.
#'  
#'  @param object object of class \code{\link[=prevR-class]{prevR}}.
#'  @param N integer or list of integers corresponding to the rings to use.
#'  @param R integer or list of integers corresponding to the rings to use.
#'  @param nb.cells number of cells on the longuest side of the studied area 
#'    (unused if \code{cell.size} is defined).
#'  @param cell.size size of each cell (in the unit of the projection).
#'  @param weighted use weighted data (TRUE, FALSE or "2")?
#'  @param plot.results plot the results?
#'  @param return.results return the results?
#'  @param return the plot within the results?
#'  @param legend.title title of the legend
#'  @param cex to control the text size on the graph
#'  @param progression show a progress bar?
#'  
#'  @details 
#'  \code{N} and \code{R} determine the rings to use for the estimation.
#'  By default, a suggested value of N will be computed with \code{\link{Noptim}}.
#'  
#'  Packages \pkg{ggplot2}, \pkg{grid} and \pkg{directlabels} are required for plotting.
#'  
#'  @return A list of one or several elements, depending on the arguments: 
#'  (i) \code{prev} is a \code{SpatialPixelsDataFrame} containing the prevalence
#'  surface; (ii) \code{radius} a \code{SpatialPixelsDataFrame} containing the
#'  kriged surface of the rings radii; (iii) \code{plot} a \code{ggplot} graph.
#'  
#'  @seealso \code{\link{Noptim}}, \code{\link{rings,prevR-method}}, 
#'  \code{\link{kde,prevR-method}}, \code{\link{krige,ANY,prevR-method}}.
#'  
#'  @examples 
#'  \dontrun{
#'    quick.prevR(fdhs)
#'  }
#'  
#'  @export
#'  @keywords spatial smooth plot

quick.prevR <- function(object, N = Noptim(object), nb.cells = 100, cell.size = NULL, weighted = NULL, plot.results = TRUE, return.results = FALSE, return.plot = FALSE, legend.title = "%", cex = 0.7, progression = TRUE) {
  if (!is.prevR(object))
    stop("object must be of class prevR.")
  if ((plot.results | return.plot) & !requireNamespace("ggplot2", quietly = TRUE))
    stop("Package ggplot2 is required for plotting.")
  if ((plot.results | return.plot) & !requireNamespace("directlabels", quietly = TRUE))
    stop("Package directlabels is required for plotting.")
  if ((plot.results | return.plot) & !requireNamespace("grid", quietly = TRUE))
    stop("Package grid is required for plotting.")
  if(is.null(weighted)) # If not precised, weighted data if available
    weighted <- "wn" %in% colnames(object@clusters)
  if (weighted & !("wn" %in% colnames(object@clusters)))
    stop("No weighted data found in object.")
  
  object <- rings(object, N=N, R=Inf, progression=progression)
  prev <- kde(object, N=N, R=Inf, weighted=weighted, nb.cells=nb.cells, cell.size=cell.size, progression=progression)
  radius <- krige(r.radius~1, object, N=N, R=Inf, nb.cells=nb.cells, cell.size=cell.size, debug.level=if (progression) -1 else 0)
  
  dimnames(prev@coords)[[2]] <- c("x","y") # Fixing name of coordinates
  dimnames(radius@coords)[[2]] <- c("x","y") # Fixing name of coordinates
  
  if((plot.results | return.plot)) {
    prev.df <- na.omit(as.data.frame(prev))
    radius.df <- na.omit(as.data.frame(radius))
    
    prev.df <- prev.df[order(prev.df$x,prev.df$y),]
    radius.df <- radius.df[order(radius.df$x, radius.df$y),]
    
    r <- data.frame()
    for (n in N) {
      if (weighted) {
        p.name <- paste0("k.wprev.N",n,".RInf")
      } else {
        p.name <- paste0("k.prev.N",n,".RInf")
      }
      r.name <- paste0("r.radius.N",n,".RInf")
      temp <- data.frame(N=n,x=prev.df[["x"]],y=prev.df[["y"]],prev=prev.df[[p.name]],radius=radius.df[[r.name]])
      r <- rbind(r,temp)
    }
    
    r$N <- factor(r$N, levels=N, labels=paste0("N=",N))
    
    p <- ggplot2::ggplot(r, ggplot2::aes(x=x, y=y, fill=prev, z=radius)) +
      ggplot2::geom_raster() +
      ggplot2::scale_fill_gradientn(colours=prevR.colors.red(20)) +
      ggplot2::stat_contour(ggplot2::aes(colour = ..level..)) +
      ggplot2::scale_colour_continuous(low = "grey50", high = "grey50") +
      ggplot2::facet_wrap(~ N) +
      ggplot2::coord_fixed() + ggplot2::xlab("") + ggplot2::ylab("") +
      ggplot2::labs(fill=legend.title) +
      theme_prevR()
  }
  
  if (plot.results) ggplot2:::print.ggplot(direct.label.prevR(p,list("top.pieces",cex=cex)))
  
  if (return.results &!return.plot)
    return(list(prev=prev, radius=radius))
  else if (return.results & return.plot)
    return(list(prev=prev, radius=radius, plot=p))
  else if (!return.results & return.plot)
    return(list(plot=p))
}
