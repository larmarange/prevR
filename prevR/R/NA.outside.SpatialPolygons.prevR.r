NA.outside.SpatialPolygons = function(sp.data, sp.poly){
  ###############################################################################################
  # Cette fonction prend un objet de la classe SpatialPixelsDataFrame et une zone d'etude definie
  # par un objet SpatialPolygons.
  # La valeur NA est affectee a chaque point de sp.data situe en-dehors de sp.poly.
  # sp.data, une fois modifie, est alors retourne
  # 
  ###############################################################################################
  
  sp.data = as(sp.data, "SpatialPixelsDataFrame")
  sp.poly = as(sp.poly, "SpatialPolygons")
  
  coords = slot(sp.data, "coords")
  is.inside = point.in.SpatialPolygons(coords[, 1], coords[, 2], sp.poly)
  
  for (i in 1:length(sp.data@data))
    sp.data@data[!is.inside, i] = NA
  
  sp.data
}