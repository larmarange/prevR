setGeneric("changeproj",
    function(object,proj){ 
        standardGeneric("changeproj") 
    }
)
setMethod("changeproj","prevR",
  function(object, proj){
  ###############################################################################################  
  # cette fonction transforme un objet de classe prevR dans une autre projection definie par proj 
  # Cette fonction modifie donc
  #   Les colonnes x et y du slot clusters
  #   Le slot boundary
  # Pour faire cela on utilise la fonction spTransform du package rgdal
  # proj peut etre une chaine de caractere ou un objet CRS
  ###############################################################################################
    if (class(proj)!="CRS")
      proj = CRS(proj)
    
    # cluster slot modification
    clusters                = slot(object,"clusters")
    coordinates(clusters)   = c("x", "y")
    proj4string(clusters)   = slot(object,"proj")
    clusters                = spTransform(clusters, proj)
    xy                      = slot(clusters,"coords")
    clusters                = slot(object,"clusters")
    clusters[,c("x","y")]   = xy
    slot(object,"clusters") = clusters
    
    # boundary slot modification
    boundary                = slot(object,"boundary")
    is.valid                = attr(boundary,"valid")
    proj4string(boundary)   = slot(object,"proj")
    boundary                = spTransform(boundary,proj)
    attr(boundary,"valid")  = is.valid
    
    N = NULL
    R = NULL
    # On verifie si rings est vide
    if(is.prevR(object,"rings")){
      rings = slot(object,"rings")
      N = sapply(rings,function(x) x$N)
      R = sapply(rings,function(x) x$R)
    }
    
    slot(object,"boundary") = boundary
    slot(object,"proj")     = proj
    slot(object,"rings")    = list()
    
    # On recalcule, le cas echeant le slot rings
    if(!is.null(N) && !is.null(R))
      object = rings(object,N=N,R=R)
    
    object
  }
)

