#'  @rdname krige,prevR-method
#'  @aliases idw-methods idw,ANY,prevR-method idw,prevR-method idw

setMethod("idw",c(formula="ANY", locations="prevR"),
  function (formula, locations, N = NULL, R = NULL, nb.cells = 100, cell.size = NULL, idp = 2,  ...)
{
  ############################################################################################### 
  # Cette fonction realise une interpolation spatiale selon l'inverse de la distance a la puissance n (argument idp) 
  # des variables contenues dans l'element rings de objects
  # idw est tres similaire a krige. l'ajustement est realise sans ajustement du semi variogram mais en precisant n (argument idp) 
  # La position du centre de chaque cercle est defini dans l'element clusters de object (colonnes x et y)
  # les arguments de cette fonction sont
  # formula : contient le nom de la ou les variables a lisser. Il peut s'agir :
  #               - une chaine de caracteres
  #               - une formule
  #               - une liste de chaines de caracteres
  #               - une liste de formules
  # Les valeurs possibles sont : r.pos, r.n, r.prev, r.radius, r.clusters, r.wpos, r.wn, r.wprev.
  # locations : un objet de la classe prevR
  # pour eviter toute confusion, la premiere commande est object=locations
  # N :  Entier ou Vecteur d integer contenant l'effectif des cercles
  # R :  Entier ou Vecteur d integer contenant le rayon des cercles
  # Le couple N-R defini un ring. Pour que les calculs soient realises il faut que l'element rings de object 
  #       contienne un ring ayant pour parametres N et R
  # nb.cells : Un entier qui contient le nombre de cellules sur la plus grande des dimensions (x ou y de clusters)
  #      On deduit facilement la taille d'une cellule donc le nombre de cellules sur la plus petite des dimensions
  # cell.size : la taille d'une cellule. Si cette valeur est fournie nb.cells est ignore
  #  idp : La puissance apparaissant dans la ponderation (1/(r puissance idp))
  # 
  ############################################################################################### 
  object=locations
  keepVariance = F # La fonction idw renvoie comme la fonction krige 2 valeurs (pred et var) mais pour idw la variance n'est
                   # pas calculee(var = NA)
  if(!is.prevR(object,"rings")) {
    stop("the slot 'rings' is empty: you have to run the rings function first.", call.=F)
  }
  
  # On accepte que l'on passe a formula une formule ou une liste de formule
  # Cependant, seuls les formules de la forme variable~1 sont acceptees
  if (class(formula)=='formula') {
    formula = list(formula)
  }
  if (class(formula)=='list') { # Si on a fourni une liste de chaines de caracteres, la classe est character
    for (i in 1:length(formula)) {
      formule = formula[[i]]
      if (class(formule)=='formula') {
        if (formule[[3]]!=1 || length(all.names(formule))!=2) {
          stop(gettextf("%s is not a valid formula: idw.prevR only accept formula with only one variable and no predictor like 'var~1'.",formule,domain="R-prevR"))
        }
        formula[[i]] = all.vars(formule) # On recupere le nom de la variable
      }
    }
    formula = as.character(formula)
  }

  clusters  = slot(object,"clusters")
  rings  = slot(object,"rings")
  if(is.null(N)) N = sapply(rings,function(x) x$N)
  if(is.null(R)) R = sapply(rings,function(x) x$R)
  .isInputOk.prevR(formula = formula, N = N, R = R)
  
  couples  = unique(data.frame(var = formula, N = N, R = R, stringsAsFactors=F))
  # couples contient les triples N R var qu'il faut lisser
  
  # on recupere la grille de points
  locations.data = as.SpatialGrid(object, nb.cells=nb.cells, cell.size=cell.size)
  
  first = T
  
  for(ic in 1:nrow(couples)){
    one.var = couples[ic,"var"]
    one.N = couples[ic,"N"]
    one.R = couples[ic,"R"]
    ringName = paste("N",one.N,".R",one.R,sep="")
    ring = rings[[ringName]]
    if(is.null(ring)) {
      warning(gettextf("no data available for the variable '%s' with N=%s and R=%s.",one.var,one.N,one.R,domain="R-prevR"))
      next
    }
    formule = formula(paste(one.var,"~1"))
    
    # dataCase est un dataframe resultat du merge de l'element clusters et du ring defini par le couple N-R
    dataCase = merge(clusters[,c("id","x","y")],ring[["estimates"]],by="id")
    coordinates(dataCase) = ~x+y
    dataCase@proj4string = object@proj
    if(nrow(dataCase)==0) next

    result.one            = idw(formule, dataCase, locations.data, idp, ...)
    gridded(result.one)   = TRUE
    temp                  = slot(result.one, "data")
    # On normalise le nom des variables resultats 
    # exemple
    #    r.prev.N100.RInf pour la prediction 
    #    r.prev.N100.RInf.var pour la prediction de la variance
    names(temp)           = c(paste(one.var,".N",one.N,".R",one.R,sep=""),paste(one.var,".N",one.N,".R",one.R,".var",sep=""))
    
    if(!keepVariance) temp = temp[,-2,drop=F]
    
    # Si on est a la premiere iteration la variable de sortie result est egale au resultat de krige  (objet de class  SpatialPixelsDataFrame)
    # Si on n'est pas a la premiere iteration on cumule a l'element data de result les elements du krigeage
    if (first) {
      slot(result.one, "data") = temp
      result = result.one
      first  = F
    } else {
      data.result = slot(result, "data")
      data.result = cbind(data.result, temp)
      slot(result, "data") = data.result
    }
  }
  
  # On passe de SpatialGridDataFrame a SpatialPixelsDataFrame
  result = as(result, "SpatialPixelsDataFrame")
  
  # Si une frontiere est definie tous les elements de la grille a l'exterieur de la grille sont positionnes a NA
  # En effet une valeur NA n'est pas tracee
  boundary     = slot(object,"boundary")
  if (attr(boundary,"valid")) {
    result = NA.outside.SpatialPolygons(result, boundary)
  }

  result
}
)
