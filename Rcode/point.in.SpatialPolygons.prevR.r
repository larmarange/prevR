point.in.SpatialPolygons = function(point.x, point.y, SpP){
  ###############################################################################################
  # Cette fonction renvoie pour chaque point defini par le couple (point.x, point.y) T ou F 
  #     si le point est a l'interieur ou non du spatialPolygons SpP
  # Un point est considere a l'interieur de N polygons si il est a l'interieur d'au moins
  # un polygon non Hole et a l'exterieur de tous les polygons Hole
  # Cette foncion est utilisee par toutes les fonctions de lissage (krige , kde, idw) . 
  # En effet ces fonctions travaillent sur un grid rectangulaire englobant les donnees. En presentation on ne veut que les resulats 
  #   interieurs a la frontiere qui est definie dans l'element SpP (SpP contient boundary). 
  #   Tous les elements du grid hors de la frontiere seront dans les programmes de lissage positonnes a NA
  # 
  ###############################################################################################

  X = slot(SpP,"polygons")
  is.inside = F
  for(i in 1:length(X)){
     PS   = slot(X[[i]],"Polygons")
     for(j in 1:length(PS)){
         pol.x = slot(PS[[j]],"coords")[,1]
         pol.y = slot(PS[[j]],"coords")[,2]
         pointsPosition = point.in.polygon(point.x, point.y, pol.x, pol.y)
         if(!slot(PS[[j]],"hole")) {
             is.inside = is.inside | pointsPosition != 0
         }
     }
  }
  is.outsideHole = T
  for(i in 1:length(X)){
     PS   = slot(X[[i]],"Polygons")
     for(j in 1:length(PS)){
         pol.x = slot(PS[[j]],"coords")[,1]
         pol.y = slot(PS[[j]],"coords")[,2]
         pointsPosition = point.in.polygon(point.x, point.y, pol.x, pol.y)
         if(slot(PS[[j]],"hole")) {
             is.outsideHole = is.outsideHole & (pointsPosition == 0 |  pointsPosition == 3)
         }
     }
  }
  is.inside & is.outsideHole
 }