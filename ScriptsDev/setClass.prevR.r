setClass(Class = "prevR",
  ###############################################################################################
  # Definition de la classe prevR
  # un objet prevR a 4 slots
  # slot clustres
  #    un data frame contenant les colonnes
  #        "id" Identifiant du cluster
  #        "x"  Longitude ou toute projection en x du cluster
  #        "y"  Longitude ou toute projection en y du cluster
  #        "n"  Le nombre de personnes entrant dans l'enquete pour ce cluster
  #        "pos" Le nombre de cas positifs pour ce cluster
  #        "wn" (facultatif) idem n mais pondere 
  #        "wpos" (facultatif) idem pos mais pondere
  #        "c.type" (facultatif) Une variable categorielle fournissant le type de chaque cluster 
  #                 Exemple c.type peut contenir le type de localisation (Urban, not Urban) du cluster
  #                 Cette information n'est utilisee que dans la fonction plot quand l'argument type = "c.type"
  #        "prev" prevalence observee 100*pos/n
  #        "wprev" (facultatif) prevalence observee ponderee 100*wpos/wn
  # slot boundary 
  #    un objet de class spatialPolygons contenant en general les frontieres d'un pays
  # slot proj
  #    une chaine de character contenant la projection dans laquelle sont exprimees les donnees
  #        c'est a dire les colonnes x et y de clusters et les polygones de boundary
  # slot rings  (Ce slot est automatiquement rempli par la fonction rings)
  #    Une liste de rings 
  # Chaque ring est une liste contenant 3 elements
  #    N : contient la valeur de N pour laquelle ce ring a ete calcule
  #    R : contient la valeur de R pour laquelle ce ring a ete calcule
  #    estimates : contient un dataframe dont les colonnes sont
  #       id : l'identificateur du cercle (qui est egal a l'identificater du cluster centre du cercle)
  #       r.pos : le nombre de cas positifs dans le cercle
  #       r.n : l'effectif du cercle
  #       r.prev : la prevalence du cercle (100*r.pos/r.n)
  #       r.radius :  le rayon du cercle
  #       r.clusters : le nombre de clusters dans le cercle
  #       Si des donnees ponderes sont presentes 
  #       r.wpos : le nombre pondere de cas positifs dans le cercle
  #       r.wn : l'effectif pondere du cercle
  #       r.wprev : la prevalence ponderee du cercle (100*r.wpos/r.wn)
  # Remarque la list rings est nommee. Le nom de chacun de ses elements est de la forme  
  #  N(valeur de n ).R(valeur de R) : exemple N500.RInf    
  # 
  # 
  ###############################################################################################      
representation(clusters = "data.frame", boundary = "SpatialPolygons", proj = "CRS", rings = "list"),
validity = function(object){
  clusters = slot(object,"clusters")
  necessaryVar = c("id","x","y","n","pos")
  ind = match(necessaryVar,names(clusters))
  if(any(is.na(ind))) {
    missing.var = paste(necessaryVar[is.na(ind)],collapse=", ")
    n.missing = length(necessaryVar[is.na(ind)])
    stop(sprintf(ngettext(n.missing,"the variable %s is missing.","the following variables (%s) are missing.",domain="R-prevR"),missing.var), call.=F)
  }
  coupledVar = c("wn","wpos")
  ind = match(coupledVar,names(clusters))
  ind = ind[!is.na(ind)]
  if(length(ind) == 1){
    stop(gettextf("the wn and wpos variables are coupled and you have only defined %s.",names(clusters)[ind],domain="R-prevR"), call.=F)
  }
  proj =  slot(object,"proj")
  isOk = try(CRS(proj@projargs),silent=T)
  if(attr(isOk,"class") == "try-error"){
    stop(gettextf("the projection %s, defined in the 'proj' argument, is incorect.",proj,domain="R-prevR"), call.=F)
  }
  boundary = slot(object,"boundary")
  boundary.proj = slot(boundary,"proj4string")
  if (boundary.proj@projargs != proj@projargs) {
    stop("'boundary' and 'clusters' didn't have the same projection.", call.=F)
  }
  T
}
)
