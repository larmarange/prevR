setGeneric("export",
    function(object, element, format, file ,N= NULL, R = NULL, clusters.only = FALSE, ext=NULL, sep=NULL, dec=NULL,  ...){ 
        standardGeneric("export") 
    }
)

setMethod("export","prevR",
  function(object, element, format, file, N= NULL, R = NULL, clusters.only = FALSE, ext=NULL, sep=NULL, dec=NULL, ...){
  ##################################################################################################
  # Cette fonction exporte dans des fichiers de differents formats les elements d'un objet de class prevR
  # les differents arguments sont
  #    object : un objet de classe prevR
  #    element   : Une chaine character contenant "clusters" ou "boundary"
  #           Si element = clusters on exporte un merge de clusters et des rings definis par N et R 
  #                    (Dans le cas ou l'argument clusters.only est positionne a T on n'exporte que clusters)
  #           Si element = boundary on exporte au format shp l'element boundary
  #   format  : une chaine de character specifiant le format du fichier exporte
  #            Cet argument n'est actif que si element = clusters
  #            Si format = dbf on exporte au format dbf
  #            Si format = txt on exporte en texte tabule (point pour les decimales)
  #            Si format = csv on exporte en csv (separateur virgule et point pour les decimales)
  #            Si format = csv2 on exporte en csv (separateur point-virgule et virgule pour les decimales)
  #            Si format = shp on exporte au format shp
  #   file  : le nom du fichier sans son extension
  #           Si le format est shp 3 fichiers aux extensions  *.shp, *.shx et *.dbf   sont crees
  #   ext :    permet de forcer l'extension du fichier cree (sans effet pour les formats shapefiles)
  #               si NULL, extension dbf pour le format dbf et txt pour le format txt.
  #   ...  : permettent de passer tous les arguments des fonctions d'ecritures
  #    
  # Les fonctions d'ecriture appelees sont
  #      writePolyShape pour l'ecriture de l'element boundary (package maptools)
  #      writePointsShape pour l'ecriture de l'element cluster (package maptools)
  #      write.table
  #      write.dbf (package foreign)
  # 
  # 
  # Exemples
  #    export(data.prevR, element="clusters", format="text", file="c:/Temp/BF6.csv",sep = ";",  clusters.only = F)
  #    export(data.prevR, element="clusters", format="shp", file="c:/Temp/BF7",clusters.only = F)
  #    export(data.prevR, element="boundary", file="c:/Temp/BF8")     
  ##################################################################################################
  # Dans ... on passe les arguments de as.data.frame
    ind = match(element,c("clusters","boundary"),nomatch=0)
    if(ind == 0){
      stop("the 'element' argument must be 'clusters' or 'boundary'.", call.=F) 
    }
    if(element=="boundary"){
      if (!require(maptools)) stop("The package maptools is required to export boundary or clusters as shapefile. Please install it.", domain="R-prevR")
      boundary = slot(object,"boundary")
      if(attr(boundary,"valid")){
        IDs <- sapply(slot(boundary, "polygons"), function(x) slot(x, "ID"))
        data <- data.frame(1, IDs,row.names=IDs)
        names(data) <- c("id", "name")
        SPDF <- SpatialPolygonsDataFrame(boundary, data)
        writePolyShape(SPDF, file, ...)
      }
      return(NULL)
    }
    
    if(element=="clusters"){
      ind = match(format,c("dbf","txt","shp","csv","csv2"),nomatch=0)
      if(ind == 0){
        stop("the 'format' argument must be 'dbf', 'txt', 'csv', 'csv2' or 'shp'.", call.=F) 
      }
      clusters = as.data.frame(object, N = N , R = R, clusters.only = clusters.only)
      if(format=="shp") {
        if (!require(maptools)) stop("The package maptools is required to export boundary or clusters as shapefile. Please install it.", domain="R-prevR")
        PS = clusters
        coordinates(PS)= ~x+y
        PS@proj4string = object@proj
        writePointsShape(PS,file, ...)
      }
      if (is.null(ext) && format!='csv2') ext = format
      if (is.null(ext) && format=='csv2') ext = 'csv'
      file = paste(file,ext,sep='.')
      if(format=="txt") {
        if (is.null(sep)) sep="\t"
        if (is.null(dec)) dec="."
        write.table(clusters, file = file,row.names = F,sep = sep, dec = dec, ...)
      }
      if(format=="csv") {
        if (is.null(sep)) sep=","
        if (is.null(dec)) dec="."
        write.table(clusters, file = file,row.names = F,sep = sep, dec = dec, ...)
      }
      if(format=="csv2") {
        if (is.null(sep)) sep=";"
        if (is.null(dec)) dec=","
        write.table(clusters, file = file,row.names = F,sep = sep, dec = dec, ...)
      }
      if(format=="dbf")  {
        if (!require(foreign)) stop("The package foreign is required to export to DBF. Please install it.", domain="R-prevR")
        write.dbf(clusters, file, ...)
      }
    }
  }
)

       