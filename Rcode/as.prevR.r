as.prevR = function(data, col,  boundary = NULL, proj = "+proj=longlat"){

##################################################################################################
# Cette fonction renvoie un objet de la classe prevR
# Les parametres d'entrees sont
#   data : un data frame quelconque
#   col : un vecteur donnant la correspondance entre le nom des variables du dataframe data et le nom des colonnes du clusters
#        Le nom des colonnes du cluster est fixe et a pour valeur
#        "id" Identifiant du cluster
#        "x"  Longitude ou toute projection en x du cluster
#        "y"  Longitude ou toute projection en y du cluster
#        "n"  Le nombre de personnes entrant dans l'enquete pour ce cluster
#        "pos" Le nombre de cas positifs pour ce cluster
#        "wn" (facultatif) idem n mais pondere 
#        "wpos" (facultatif) idem pos mais pondere
#        "c.type" (facultatif) une variable facteur de classification des clusters (utilisee selon pour cartographier les clusters)
#              exemple :  
#               col   = c(id = NULL, x = "x", y="y", n="n", wn = "nWeighted" ,wpos = "posWeighted", pos = "positifs", c.type = NULL)
#               Seule la presence de x,y,n,pos est obligatoire
#               Si id est manquant une variable id est creee contenant 1,2,3,etc
#        Par ailleurs, la fonction ajoutera deux autres variables  :
#           "prev" egale a 100*pos/n
#           "wprev" egale a 100*wpos/wn
#
#   boundary : un objet de classe SpatialPolygons , il peut etre NULL. 
#   Si il est NULL la fonction cree un polygon (classe spatialPolygons) qui est tout simplement un rectangle incluant tous les points de data. 
#   On affecte a boundary un attribut 'valid' que l'on positionne a F. 
#
#   proj : De classe character il contient la projection dans laquelle  les donnees  x, y et boundary sont exprimees
#          Peut egalement etre un objet de la classe CRS.
#          Pour plus d'information rendez vous sur le site de proj4 http://trac.osgeo.org/proj/
#          Attention aucune transformation n'est realisee ici, cette variable est juste informative
# 
##################################################################################################


  if(!is.data.frame(data)) {
    stop("the 'data' argument must be a dataframe.", call.=F)
  }
  ind         = match(col,names(data))
  if(any(is.na(ind))) {
    missing.var = paste(col[is.na(ind)],collapse=", ")
    n.missing = length(col[is.na(ind)])
    sprintf(ngettext(n.missing,"the variable %s, defined in 'col', is not present in 'data'.","the following variables (%s), defined in 'col', are not present in 'data'.",domain="R-prevR"),missing.var) -> stop.mess
    stop(stop.mess, call.=F)
  }
  col         = col[!is.null(col)]
  data        = data[,col]
  names(data) = names(col)
  necessaryVar = c("id","x","y","n","pos")
  ind = match(necessaryVar,names(data))
  if(any(is.na(ind))) {
    missing.var = paste(necessaryVar[is.na(ind)],collapse=", ")
    n.missing = length(necessaryVar[is.na(ind)])
    sprintf(ngettext(n.missing,"the variable %s is missing in 'col'.","the following variables (%s) are missing in 'col'.",domain="R-prevR"),missing.var) -> stop.mess
    stop(stop.mess, call.=F)
  }
  if(!is.element("id",names(data))){
    data = cbind(id = 1:nrow(data) , data)
  }
  utilsVar    = c("id", "x", "y", "n", "pos", "prev", "wn", "wpos", "wprev", "c.type")
  ind         = match(names(data), utilsVar,nomatch=0)
  if(any(ind==0)) {
    cancelled.var = paste(names(data)[ind==0],collapse=", ")
    n.cancelled = length(names(data)[ind==0])
    sprintf(ngettext(n.cancelled,"The variable %s has been cancelled from 'data'.","The following variables (%s) have been cancelled from 'data'.",domain="R-prevR"),cancelled.var) -> mess
    message(mess)
  }
  # Reduire data apres avoir calcule le message d'information.
  data        = data[,ind!=0]
  
  # On force c.type a etre du type factor
  if (!is.null(data$c.type)) {
    data$c.type = as.factor(data$c.type)
  }
  
  # On calcule si besoin prev et wprev
  if (is.null(data$prev)) {
    data$prev=100*data$pos/data$n
  }
  if (is.null(data$wprev) && !is.null(data$wpos) && !is.null(data$wn)) {
    data$wprev=100*data$wpos/data$wn
  }
  
  # proj peut eventuellement etre un objet de la classe CRS
  if(is(proj,"CRS")) {
    proj = proj@projargs
  }
  isOk = try(CRS(proj),silent=T)
  if(attr(isOk,"class") == "try-error"){
    stop(gettextf("the projection %s, defined in the 'proj' argument, is incorect.",proj,domain="R-prevR"), call.=F)
  }
  projCRS = CRS(proj)
  proj    = slot(projCRS,"projargs")
  # Si boundary n'existe pas il faut en creer un fictif pour que le slot boundary de la classe prevR puisse etre renseigne
  # On cree donc un objet de classe spatialPolygons fictif et on lui donne un attribut "valid" que l'on positionne a F
  if(is.null(boundary)){
    x                = data[,"x"]
    y                = data[,"y"]
    xx               = c(min(x),min(x),max(x),max(x),min(x))
    yy               = c(min(y),max(y),max(y),min(y),min(y))
    boundary         = Polygon(cbind(xx,yy))
    boundary         = Polygons(list(boundary),"P1")
    boundary         = SpatialPolygons(list(boundary))
    attr(boundary,"valid") = F
    slot(boundary,"proj4string") = projCRS
  } else {
    if(class(boundary) != "SpatialPolygons") {
      stop("the class of 'boundary' must be SpatialPolygons.", call.=F)
    }
    # On teste si boundary contient une projection.
    # Si pas de projection, on suppose que boundary est dans la meme projection que clusters.
    # Sinon, on transforme boundary a la volee pour le passer dans la meme projection que clusters
    if (is.na(boundary@proj4string@projargs)) {
      slot(boundary,"proj4string") = projCRS
      message(gettextf("No projection was defined in 'boundary' argument: 'boundary' has then be considered to be in the same projection (%s) as 'data'.",proj,domain="R-prevR"))
    } else {
      boundary = spTransform(boundary,projCRS)
    }
    attr(boundary,"valid") = T
  }
  new("prevR",clusters = data, proj = projCRS, boundary = boundary,rings = list())
}
