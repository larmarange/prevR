#' Spatial interpolation (kriging and inverse distance weighting) for objects of class prevR.
#' 
#' These functions execute a spatial interpolation of a variable of the slot \code{rings} of 
#' an object of class [prevR-class]. The method \code{krige} implements 
#' the ordinary kriging technique. The method \code{idw} executes an inverse distance weighting 
#' interpolation.
#' 
#' @param formula variable(s) to interpolate (see details).
#' @param locations object of class [prevR-class].
#' @param N integer or list of integers corresponding to the rings to use.
#' @param R integer or list of integers corresponding to the rings to use.
#' @param model a variogram model returned by the function [gstat::vgm()].
#' @param nb.cells number of cells on the longest side of the studied area 
#'   (unused if \code{cell.size} is defined).
#' @param cell.size size of each cell (in the unit of the projection).
#' @param fit `"auto"` for using a variogram automatically fitted from the data, 
#'   only if \code{model} is not defined (`NULL`). DEPRECATED: as `geoR` package
#'   has been removed from CRAN, `"manual"` option is no longer available.
#' @param keep.variance return variance of estimates?
#' @param show.variogram plot the variogram?
#' @param  idp inverse distance weighting power (see [gstat::idw()]).
#' @param \dots additional arguments transmitted to [gstat::krige()]
#'   or [gstat::idw()].
#'   
#' @import sp
#' @importMethodsFrom gstat krige
#' @importFrom gstat variogram
#' @importFrom gstat vgm
#' @importFrom gstat fit.variogram
#'   
#' @details \code{formula} specifies the variable(s) to interpolate. Only variables available in the 
#' slot \code{rings} of \code{locations} could be used. Possible values are "r.pos", "r.n", "r.prev", 
#' "r.radius", "r.clusters", "r.wpos", "r.wn" or "r.wprev". Variables could be specified with a character 
#' string or a formula (example: \code{list(r.pos~1,r.prev~1}). Only formula like \code{variable.name~1} 
#' are accepted. For more complex interpolations, use directly functions [gstat::krige()] and
#' [gstat::idw()] from \pkg{gstat}.
#' 
#' \code{N} and \code{R} determine the rings to use for the interpolation. If they are not defined, 
#' surfaces will be estimated for each available couples (N,R). Several interpolations could be 
#' simultaneously calculated if several variables and/or several values of N and R are defined.
#' 
#' A suggested value of N could be computed with [Noptim()].
#' 
#' In the case of an ordinary kriging, the method [krige()] from \pkg{prevR} will try to fit automatically
#' a exponential variogram to the sample variogram (\code{fit="auto"}). You can also specify
#' directly the variogram to use with the parameter \code{model}. 
#' 
#' Interpolations are calculated on a spatial grid obtained with 
#' [as.SpatialGrid()].
#' 
#' @return Object of class [sp::SpatialPixelsDataFrame-class]. 
#' The name of estimated surfaces depends on the name of the interpolated variable, N and R 
#' (for example: \emph{r.radius.N300.RInf}). If you ask the function to return variance 
#' (\code{keep.variance=TRUE}), corresponding surfaces names will have the suffix \emph{.var}.
#' 
#' \code{NA} value is applied to points located outside of the studied area \cr
#' (see [NA.outside.SpatialPolygons()]).
#' 
#' @references
#' Larmarange Joseph, Vallo Roselyne, Yaro Seydou, Msellati Philippe and Meda Nicolas (2011) 
#' "Methods for mapping regional trends of HIV prevalence from Demographic and Health Surveys (DHS)", 
#' \emph{Cybergeo: European Journal of Geography}, no 558, \url{https://journals.openedition.org/cybergeo/24606}, 
#' DOI: 10.4000/cybergeo.24606.
#' 
#' @note Results could be plotted with [sp::spplot()].\cr
#' \pkg{prevR} provides several continuous color palettes (see [prevR.colors]) compatible 
#' with [sp::spplot()].\cr
#' Calculated surfaces could be export using the function 
#' [maptools::writeAsciiGrid()].
#' 
#' @seealso [gstat::krige()], [gstat::idw()], 
#' [rings()], [Noptim()].
#' 
#' @examples 
#'   \dontrun{
#'     dhs <- rings(fdhs, N = c(100,200,300,400,500))
#'     radius.N300 <- krige('r.radius', dhs, N = 300, nb.cells = 200)
#'     prev.krige <- krige(r.wprev ~ 1, dhs, N = c(100, 300, 500))
#'     library(sp)
#'     spplot(prev.krige, c('r.wprev.N100.RInf', 'r.wprev.N300.RInf', 'r.wprev.N500.RInf'))
#'   }
#' 
#' @keywords smooth spatial
#' @exportMethod krige
#' @aliases krige,prevR-method krige-methods krige,ANY,prevR-method krige

setMethod("krige",c(formula="ANY", locations="prevR"),
  function (formula , locations, N = NULL, R = Inf, model = NULL, nb.cells = 100, cell.size = NULL, fit = "auto", keep.variance = FALSE,  show.variogram = FALSE,   ...)
  {
 ###############################################################################################
  # Cette fonction realise un krigeage des variables contenues dans l'element rings de objects
  # La position du centre de chaque cercle est defini dans l'element clusters de object (colonnes x et y)
  # les arguments de cette fonction sont
  # formula : contient le nom de la ou les variables a lisser. Il peut s'agir :
  #               - une chaine de caracteres
  #               - une formule
  #               - une liste de chaines de caracteres
  #               - une liste de formules
  # Les valeurs possibles sont : r.pos, r.n, r.prev, r.radius, r.clusters, r.wpos, r.wn, r.wprev.
  # locations : un objet de la classe prevR
  # N :  Vecteur d integer contenant l'effectif des cercles
  # R :  Vecteur d integer contenant le rayon des cercles
  # Le couple N-R definit un ring. Pour que les calculs soient realises il faut que l'element rings de object 
  #       contienne un ring ayant pour parametres N et R
  # model : list contenant les modeles de semi variogram de chacun des ajustements (regarder la fonction vgm du package gstat)
  # nb.cells : Un entier qui contient le nombre de cellules sur la plus grande des dimensions (x ou y de clusters)
  #      On deduit facilement la taille d'une cellule donc le nombre de cellules sur la plus petite des dimensions
  # cell.size : la taille d'une cellule. Si cette valeur est fournie nb.cells est ignore
  # fit : Une chaine de characters precisant si l'ajustement du semi variogram doit etre effectue de facon "auto" ou "manual" 
  #      si fit = "auto" un modele exponentiel est ajuste
  # Remarque Si l'argument model n'est pas NULL, l'argument fit n'est pas pris en compte
  #          La longueur de la liste model doit etre compatible avec la taille de N , de R de var .(Pour plus d'information regarder la fonction .isInputOk.prevR)
  # keep.variance : Un logical
  #           la fonction krige du package gstat fournie 2 resultats (prediction et prediction variance) si
  #        keep variance = T la fonction renvoie prediction et prediction variance 
  #        keep variance = F la fonction ne renvoie pas prediction variance
  # show.variogram : un logical si 
  #        show.variogram = T . Un graphique presentant les ajustements des semi variograms est realise  
  #        show.variogram = F . Pas de graphique presentant les ajustements des semi variograms.
  #
  ############################################################################################### 
  
    object=locations
    if(!is.prevR(object,"rings")) {
      stop("the slot 'rings' is empty: you have to run the rings function first.", call.=F)
    }
  
    # On accepte que l'on passe a formula une formule ou une liste de formule
    # Cependant, seuls les formules de la forme variable~1 sont acceptees
    if (inherits(formula, 'formula')) {
      formula = list(formula)
    }
    if (is.list(formula)) { # Si on a fourni une liste de chaines de caracteres, la classe est character
      for (i in 1:length(formula)) {
        formule = formula[[i]]
        if (inherits(formule, 'formula')) {
          if (formule[[3]]!=1 || length(all.names(formule))!=2) {
            stop(gettextf("%s is not a valid formula: idw.prevR only implement simple or ordinary kriging, so formula must have only one variable and no predictor (like 'var~1').",formule,domain="R-prevR"))
          }
          formula[[i]] = all.vars(formule)  # On recupere le nom de la variable
        }
      }
      formula = as.character(formula)
    }
    if(is.null(model) && !is.element(fit,(c("auto")))){
       stop("the 'fit' argument must be 'auto'.")
    }
    if(inherits(model, "variogramModel")) model = list(model)
    
    clusters  = slot(object,"clusters")
    rings  = slot(object,"rings")
    if(is.null(N)) N = sapply(rings,function(x) x$N)
    if(is.null(R)) R = sapply(rings,function(x) x$R)
    
    if(length(model)!=0) {
        .isInputOk.prevR(formula = formula, N = N, R = R, model = model)
    } else {
        .isInputOk.prevR(formula = formula, N = N, R = R)
    }
    couples  = unique(data.frame(var = formula, N = N, R = R, stringsAsFactors=F))
    if(length(model)==1) model = rep(model,nrow(couples))
    # on recupere la grille de points
    locations.data = as.SpatialGrid(object, nb.cells=nb.cells, cell.size=cell.size)
    
    i                           = 0
    list.variogram              = list()
    first                       = T
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

      dataCase = merge(clusters[,c("id","x","y")],ring[["estimates"]],by="id")
      coordinates(dataCase) = ~x+y
      dataCase@proj4string = object@proj
      if(nrow(dataCase)==0) next
      if(length(model)!=0) one.model = model[[ic]]
      if(is.null(model) && fit == "auto"){
         sample.vario   = gstat::variogram(formule, dataCase)
         param1Vgm      = max(sample.vario[,"gamma"])*0.66
         param2Vgm      = max(sample.vario[,"dist"])*0.5
         param          = .init.exp.model.variogram(sample.vario[,"dist"],sample.vario[,"gamma"])
         if(is.null(param)){
            warning(gettextf("problem to fit the variogram: the variable '%s' width N=%s and R=%s has not been treated.",one.var,one.N,one.R,domain="R-prevR"))
            next
         }
         one.model      = try(gstat::fit.variogram(sample.vario, model = gstat::vgm(param[1],'Exp',param[2])),silent =T ) 
         if(inherits(one.model, "try-error") || attr(one.model,"singular")) one.model = gstat::vgm(param[1],'Exp',param[2])
      }

      result.one            = krige(formule, dataCase, locations.data, model = one.model, ...)
      if(length(model)==0) {
         i = i+1
         list.variogram[[i]] = list(parameters = c(one.var, one.N, one.R), sample.vario = sample.vario, model = one.model)
      }
      gridded(result.one)   = TRUE
      temp                  = slot(result.one, "data")
      names(temp)           = c(paste(one.var,".N",one.N,".R",one.R,sep=""),paste(one.var,".N",one.N,".R",one.R,".var",sep=""))
      if(!keep.variance) temp = temp[,-2,drop=F]
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
    if (show.variogram) {
      n1 = 1
      n2 = 1
      more = T
      for (k in 1:length(list.variogram)) {
        nn = ceiling(sqrt(length(list.variogram)))
        ll = list.variogram[[k]]
        key = list(columns=3,  text = list(paste(c(" ","N :","R :"),ll$param)))
        if(k == length(list.variogram)) more = F
        print(plot(ll$sample.vario,ll$model,key=key),split=c(n1,n2,nn,nn),more=more)
        n1 = n1 + 1
        if(n1 == nn+1) {
         n1 = 1
         n2 = n2 +1
        }
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


#' @rdname krige-ANY-prevR-method
#' @aliases idw-methods idw,ANY,prevR-method idw,prevR-method idw
#' @importFrom gstat idw
#' @exportMethod idw

setMethod("idw",c(formula="ANY", locations="prevR"),
          function (formula, locations, N = NULL, R = Inf, nb.cells = 100, cell.size = NULL, idp = 2,  ...)
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
            if (inherits(formula, 'formula')) {
              formula = list(formula)
            }
            if (is.list(formula)) { # Si on a fourni une liste de chaines de caracteres, la classe est character
              for (i in 1:length(formula)) {
                formule = formula[[i]]
                if (inherits(formule, 'formula')) {
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
              
              result.one            = gstat::idw(formule, dataCase, locations.data, idp, ...)
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
