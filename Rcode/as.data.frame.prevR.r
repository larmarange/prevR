as.data.frame.prevR = function(x,..., N=NULL, R=NULL, clusters.only=FALSE){
  ###############################################################################################
  # Cette fonction renvoie un data frame qui est soit 
  #   le slot clusters (argument clusters.only = T)
  #   un merge des slots clusters et rings 
  #     (Dans ce cas ne sont transformes que les rings correspondant aux valeurs de N et R fournies en arguments)
  #     Si aucune valeur de N et de R n'est fournies , l'ensembes des slots rings sont assemblees (merge) avec clusters
  ###############################################################################################
  out = NULL
  if(!clusters.only && is.prevR(x,"rings")){
    rings = slot(x,"rings")
    if(is.null(N)) N = sapply(rings,function(x) x$N)
    if(is.null(R)) R = sapply(rings,function(x) x$R)
    .isInputOk.prevR(N = N, R = R)
    couples  = unique(data.frame(N = N, R = R, stringsAsFactors=F))
    ringsNames = paste("N",couples[,1],".R",couples[,2],sep="")
    ind = match(ringsNames, names(rings),nomatch = 0) 
    if(sum(ind!=0)) {
      ringsNames = names(rings)[ind] 
      ring       = slot(x,"rings")[[ringsNames[1]]]
      clusters   = slot(x,"clusters")
      out = merge(clusters,ring$estimates,by="id")
      if(length(ringsNames) > 1){
        for(one.ring in ringsNames[-1]){
          ring  = slot(x,"rings")[[one.ring]]
          out   = merge(out,ring$estimates,by="id")
        }
      }
      indId      = match("id",names(ring$estimate))
      namesCol   = c(names(clusters),outer(names(ring$estimate)[-indId],ringsNames,paste,sep="."))
      names(out) = namesCol
    }
  }
  if(clusters.only || !is.prevR(x,"rings")){
    out = slot(x,"clusters")
  }
  out
}

 
