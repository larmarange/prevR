setGeneric("as.SpatialGrid",
    function(object, nb.cells = 100, cell.size = NULL){ 
        standardGeneric("as.SpatialGrid") 
    }
)
setMethod("as.SpatialGrid","prevR",
  # nb.cells : Un entier qui contient le nombre de cellules sur la plus grande des dimensions (x ou y de clusters)
  #      On deduit facilement la taille d'une cellule donc le nombre de cellules sur la plus petite des dimensions
  # cell.size : la taille d'une cellule. Si cette valeur est fournie nb.cells est ignore
  function (object, nb.cells = 100, cell.size = NULL){
  # calcul de la taille des cellules et donc du nombre de cellules sur chaque coordonnees
  boundary     = slot(object,"boundary")
  if (!attr(boundary,"valid")) {
    max.x = max(clusters[["x"]])
    min.x = min(clusters[["x"]])
    max.y = max(clusters[["y"]])
    min.y = min(clusters[["y"]])
  } else {
    bbox  = slot(boundary,"bbox")
    max.x = bbox["x","max"]
    min.x = bbox["x","min"]
    max.y = bbox["y","max"]
    min.y = bbox["y","min"]
  }
  dx = max.x - min.x
  dy = max.y - min.y
  if(is.null(cell.size)){
    dxy = max(c(dx,dy))
    cell.size = dxy/nb.cells
    nb.cells.x = dx%/%cell.size
    nb.cells.y = dy%/%cell.size
  } else {
    nb.cells.x = dx%/%cell.size + 1
    nb.cells.y = dy%/%cell.size + 1
  }

# Creation de la grille sur laquelle aura lieu le lissage
  grid_topo = GridTopology(c(min.x, min.y), c(cell.size, cell.size), c(nb.cells.x, nb.cells.y))
  SG = SpatialGrid(grid_topo, proj4string=object@proj)
  return(SG)
}
)