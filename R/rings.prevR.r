setGeneric("rings",
    function(object, N = seq(100, 500, 50), R = Inf, progression = TRUE){
        standardGeneric("rings")
    }
)
setMethod("rings","prevR",
  function (object, N = seq(100, 500, 50), R = Inf, progression = TRUE)
  {
  ###############################################################################################
  # Cette fonction calcule la prevalence (et la prevalence ponderee) par la methode des cercles.
  # Cette fonction ajoute chaque ring defini par un couple N R a la list du slot rings de object
  # C'est a dire que si le slot rings de object n'est pas NULL alors les nouveaux resulats
  #    de calul viennent s'ajouter a la liste contenue dans l'element rings de object
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
  # Besoin du package fields pour rdist et rdist.earth   
  #
  ###############################################################################################

# On teste la coherence des donnees d'entree N et R Pour plus de precision regardez la fonction .isInputOk.prevR
    .isInputOk.prevR(N =N, R = R)
    clusters = slot(object,"clusters")
    boundary = slot(object,"boundary")
    coord.clust = clusters[,c("x","y")]

    projCRS = slot(object,"proj")
    proj    = slot(projCRS,"projargs")
# Si les donnees georeferencees sont exprimees en longitude latitude les distances entre clusters sont calculees en Km
# par la fonction rdist.earth du package fields
# Autrement les distances sont calculees par la fonction rdist (fields) en unite des donnees de depart
    if(regexpr("longlat",proj)==-1 && regexpr("latlong",proj)==-1){
      distances = rdist(coord.clust)
    } else {
      distances = rdist.earth(coord.clust,miles=F)
    }
    rings   = slot(object,"rings")
# On cree les couple N-R a partir desquels seront calclules les rayons des cercles
# et par consequent l'effectif et la prevalence dans chaque cercle 
    couples = cbind(N=N,R=R)
# Barre de progression
    if (progression) {
      message("Progress of calculations:",domain="R-prevR")
      barre = txtProgressBar(min=0, max=nrow(couples)*nrow(clusters), initial=0, style=3)
    }
# boucle sur les couples  N R 
    for (ic in 1:nrow(couples)) {
      one.N = couples[ic,"N"]
      one.R = couples[ic,"R"]

      result  = data.frame()
# boucle sur les clusters
      for (i in 1:nrow(clusters)) {
        one.clust = clusters[i, ]
        temp = clusters
        temp$dist = distances[i, ]
        temp = temp[order(temp$dist), ]

        temp$cum.n = cumsum(temp[["n"]])
        if (length(temp[temp$cum.n >= one.N, ]$cum.n) == 0){
          maxi = Inf
        } else {
          maxi = min(temp[temp$cum.n >= one.N, ]$cum.n)
        }
        temp = temp[temp$cum.n <= maxi, ]
#
# A ce niveau temp contient un sous dataframe de clusters qui est tel que
# Les clusters (c'est a dire les lignes de temp) sont classes en fonction de la distance au cluster d'origine (one.clust)
#    du plus pres au plus loin
# On ne garde que les clusters qui sont tels que l'effectif des clusters dans le cercle est de l'ordre de N sous la contrainte
#    que le rayon doit etre inferieur a R
# 

        temp2      = temp[temp$dist <= one.R, ]
        one.result = data.frame(id = one.clust[,"id"])
# r.pos : le nombre de cas positifs dans le cercle
# r.n : l'effectif du cercle
# r.prev : la prevalence du cercle (100*r.pos/r.n)
# r.radius :  le rayon du cercle
# r.clusters : le nombre de clusters dans le cercle 
        one.result$r.pos             = sum(temp2[["pos"]],na.rm=T)
        one.result$r.n               = sum(temp2[["n"]],na.rm=T)
        one.result$r.prev            = 100*one.result$r.pos/one.result$r.n
        if ( one.R != Inf && one.N == Inf) {
          one.result$r.radius          = R
        } else {
          one.result$r.radius          = max(temp2$dist)
        }
        one.result$r.clusters        = length(temp2$dist)

# Si des donnees ponderes sont presentes on calule
# r.wpos : le nombre pondere de cas positifs dans le cercle
# r.wn : l'effectif pondere du cercle
# r.wprev : la prevalence ponderee du cercle (100*r.wpos/r.wn)
        wn     = temp2[["wn"]]
        if(!is.null(wn)) {
          one.result$r.wpos  = sum(temp2[["wpos"]])
          one.result$r.wn    = sum(temp2[["wn"]])
          one.result$r.wprev = 100*one.result$r.wpos/one.result$r.wn
        }
        result = rbind(result, one.result)
        
        if (progression) {
          barre.cur = (ic-1)*nrow(clusters)+i
          setTxtProgressBar(barre,value=barre.cur)
        }
        
      }
      rings[[paste("N",one.N,".R",one.R,sep="")]] = list(N=one.N,R=one.R,estimates=result)
    }

    if (progression) {
      close(barre)
    }
    slot(object,"rings") = rings
    return(object)
  }
)