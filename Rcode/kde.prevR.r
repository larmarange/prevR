setGeneric("kde",
    function(object,  N = NULL, R = NULL, weighted = TRUE, risk.ratio = FALSE, keep.details = FALSE, nb.cells = 100, cell.size = NULL, progression=TRUE){
        standardGeneric("kde")
    }
)
setMethod("kde","prevR",
  function(object,  N = NULL, R = NULL, weighted = TRUE, risk.ratio = FALSE, keep.details = FALSE, nb.cells = 100, cell.size = NULL, progression=TRUE) {  
  ###############################################################################################
  # Cette fonction calcule une surface de prevalence a partir du ratio de  2 surfaces de densites
  #   calculees par la methode  des estimateurs a noyaux ( noyaux Gaussiens)
  # Cette fonction est base sur la fonction KernSur du package GenKern  
  # La position du centre de chaque cercle est defini dans l'element clusters de object (colonnes x et y)
  # La largeur de bande est egale a la moitie du rayon des cercles. Dans le cas de donnees en longlat un 
  #  calcul supplementaire est necessaire
  # les arguments de cette fonction sont
  # object : Un objet de class prevR. 
  # N :  Vecteur d integer contenant l'effectif des cercles
  # R :  Vecteur d integer contenant le rayon des cercles
  # Le couple N-R defini un ring. Pour que les calculs soient realises il faut que l'element rings de object 
  #       contienne un ring ayant pour parametres N et R
  #
  # weigted = T (defaut) : On calcule les resultats ponderes
  #         = F          : On calcule les resultats non ponderes
  #         = 2          : On clacule les resultats pondere et non ponderes
  # Si weigted = T et si object ne contient pas de ponderation  on positionne weighted a F + message
  #
  # risk.ratio = F (defaut) On calcule (k.pos  k.obs  k.prev) ou/et (k.wpos  k.wobs  k.wprev)
  # risk.ratio = T          On calcule (k.case k.control k.rr) ou/et (k.wcase k.wcontrol k.wrr)
  # risk.ratio = 2          On calcule (k.pos  k.obs  k.prev k.case k.cont k.rr) ou/et (k.wpos  k.wobs  k.wprev k.wcase k.wcont k.wrr)
  # Remarque pos, n ,wpos, wn sont elements de clusters
  #
  # keep.details T          on sort tous les resultats calcules
  #              F (defaut) Parmi les resultats calcules on ne sort que k.prev, k.wprev, k.rr, k.wrr
  #
  # Les valeurs calculees sont 
  # k.case     = k.pos / sum(pos)
  # k.control  = k.obs / sum(n)
  # k.wcase    = k.wpos / sum (wpos)
  # k.wcontrol = k.wobs / sum (wobs)
  # k.rr       = k.case / k.control   = k.prev * sum (wobs)/sum(wpos)
  # k.wrr      = k.wcase / k.wcontrol = k.wprev * sum (wobs)/sum(wpos)
  # 
  # avec         k.pos[j,k]  = sum sur i de dens[i,j,k] * pos[i] 
  #              k.obs[j,k]  = sum sur i de dens[i,j,k] * n[i] 
  #              i indice du cercle ,
  #              j,k indices de la grille
  #              pos[i] nombre de cas positifs dans le cercle i 
  #              n[i]   nombre d'individus dans le cercle i  
  #              dens[j,k] densite au pont j,k de centre x[i] y[i] de largeur de bande = radius[i]/2
  #                        dens est calcule par la fonction KernSur
  #
  #
  ###############################################################################################
    if(!is.prevR(object,"rings")) {
      stop("the slot 'rings' is empty: you have to run the rings function first.", call.=F)
    }

    clusters  = slot(object,"clusters")
    ind = match(c("wpos","wn"),names(clusters),nomatch=0)
    if((weighted == T | weighted == 2) && any(ind==0)) {
       warning("'wpos' and 'wn' are not present: the 'weighted' argument has been put at FALSE.")
       weighted = F
     }
    rings  = slot(object,"rings")
    if(is.null(N)) N = sapply(rings,function(x) x$N)
    if(is.null(R)) R = sapply(rings,function(x) x$R)
    .isInputOk.prevR(N = N, R = R)
    couples  = unique(data.frame(N = N, R = R, stringsAsFactors=F))
    # couples contient les triples N R var qu'il faut lisser
    
    # Est-on en longitude/latitude ? 
    boundary     = slot(object,"boundary")
    proj = slot(slot(object,"proj"),"projargs")
    longlat = F
    if(regexpr("longlat",proj) != -1 || regexpr("latlong",proj) != -1){
      longlat = T
    }
    
    # Calcul de la grille
    coord = coordinates(as.SpatialGrid(object, nb.cells=nb.cells, cell.size=cell.size))
    range.x = unique(coord[,1])
    range.y = unique(coord[,2])
    
    first = T
    one.var="r.prev"
    result=NULL

    # Barre de progression
    if (progression) {
      message("Progress of calculations:",domain="R-prevR")
      barre = txtProgressBar(min=0, max=nrow(couples)*nrow(clusters)+25, initial=0, style=3)
      # On ajoute 25 au max pour prendre en compte le temps de transformation en data.frame a la fin de la fonction
    }
    
    for(ic in 1:nrow(couples)){
      one.N = couples[ic,"N"]
      one.R = couples[ic,"R"]
      ringName = paste("N",one.N,".R",one.R,sep="")
      ring = rings[[ringName]]
      if(is.null(ring)) {
        warning(gettextf("no data available for the variable '%s' with N=%s and R=%s.",one.var,one.N,one.R,domain="R-prevR"))
        next
      }

      dataCase = merge(clusters,ring[["estimates"]],by="id")
      if(nrow(dataCase)==0) next
      bw   = dataCase[["r.radius"]]
      bwx  = bw
      bwy  = bw
      x    = dataCase[["x"]]
      y    = dataCase[["y"]]

      # Attention si x et y sont en degres il faut calculer la largeur de bande  en Km 
      # 6378.388 correspond au rayon terrestre moyen
      if(longlat){
        bwx = bw/(6378.388*cos(pi*y/180))
        bwx = bwx*180/pi
        bwy = bw/6378.388
        bwy = bwy*180/pi
      }
      k.pos = 0
      k.obs = 0
      k.wpos = 0
      k.wobs = 0
      for (i in 1:length(bw)) {
        temp   = KernSur(x=x[i],y=y[i],xbandwidth=bwx[i]/2,ybandwidth=bwy[i]/2,range.x=range.x,range.y=range.y)
        k.pos  = k.pos + temp$zden * dataCase[["pos"]][i]
        k.obs  = k.obs + temp$zden * dataCase[["n"]][i]
        if(weighted == T || weighted == 2){
          k.wpos = k.wpos + temp$zden * dataCase[["wpos"]][i]
          k.wobs = k.wobs + temp$zden * dataCase[["wn"]][i]
        }
        if (progression) {
          barre.cur = (ic-1)*nrow(clusters)+i
          setTxtProgressBar(barre,value=barre.cur)
        }
      }
      k.case     = k.pos  / sum(clusters[["pos"]])
      k.control  = k.obs  / sum(clusters[["n"]])
      k.prev     = 100*k.pos/k.obs
      k.rr       = 100*k.case/k.control
      if(weighted == T || weighted == 2){
        k.wcase    = k.wpos / sum(clusters[["wpos"]])
        k.wcontrol = k.wobs / sum(clusters[["wn"]])
        k.wprev    = 100*k.wpos/k.wobs
        k.wrr      = 100*k.wcase/k.wcontrol
      }
      
      result.one = NULL
      
      if(risk.ratio == F || risk.ratio == 2){
        result.one = c(result.one, list(k.pos=k.pos, k.obs=k.obs, k.prev=k.prev))
        if(weighted == T || weighted == 2){
          result.one = c(result.one, list(k.wpos=k.wpos, k.wobs=k.wobs, k.wprev=k.wprev))
        }
      }
      if(risk.ratio == T || risk.ratio == 2){
        result.one = c(result.one, list(k.case=k.case, k.control=k.control, k.rr=k.rr))
        if(weighted == T || weighted == 2){
          result.one = c(result.one, list(k.wcase=k.wcase, k.wcontrol=k.wcontrol, k.wrr=k.wrr))
        }
      }
      
      varNames = names(result.one)
      if(!keep.details){
        ind = match(c("k.prev","k.wprev","k.rr","k.wrr"), varNames,nomatch=0)
        result.one = result.one[ind]
      }
      
      names(result.one) = c(paste(names(result.one),".N",one.N,".R",one.R, sep = ""))
      if(is.null(result)) {
        result = result.one
      } else {
        result = c(result,result.one)
      }
    }
    
    # Ajout des coordonnees
    result = c(list(x=temp$xords, y=temp$yords),result)
    
    # Passage en data.frame
    result = xyz2dataframe(result,'x','y',names(result)[c(-1,-2)])
    if (progression) {
      barre.cur = nrow(couples)*nrow(clusters)+15
      setTxtProgressBar(barre,value=barre.cur)
    }
    # puis en SpatialPixelsDataFrame
    result = SpatialPixelsDataFrame(points=result[,c('x','y')], data=result[,c(-1,-2)])
    result@proj4string = object@proj
    
    # Suppression des points hors de la zone d'etude
    if (attr(boundary,"valid")) {
      result = NA.outside.SpatialPolygons(result, boundary)
    }
    
    if (progression) {
      barre.cur = nrow(couples)*nrow(clusters)+25
      setTxtProgressBar(barre,value=barre.cur)
      close(barre)
    }
    
    result
  }
)

