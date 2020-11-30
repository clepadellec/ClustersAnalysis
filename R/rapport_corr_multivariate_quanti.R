#' Title
#'
#' @param data a datafram or matrix with quantitative value
#' @param g a factor such that length(x)=nrow(data)
#'
#' @return rapport correlation
#'
#' @import FactoMineR
#' @export
#'
#' @examples
R2_multivariate=function(data,g, method='encoding'){

  if (data_type(data)=='qualitatives'){
    if (method=='encoding'){
    data_bis=dummy_data(data)
    } else{
      p=ncol(data)
      M=sum(sapply(data, FUN = function(x){return(length(unique(x)))}))
      n_acm=M-p
      ACM=MCA(data, ncp = n_acm, graph = FALSE)
      data_bis=ACM$ind$coord
    }
  } else{
    data_bis=data
  }




  G=apply(data_bis, MARGIN = 2, FUN = mean)
  n_g=tapply(data_bis[,1],g,FUN = length)
  n_G=length(unique(g))
  columns=ncol(data_bis)
  row=nrow(data_bis)
  barycentre=c()
  for (i in 1:columns){
    barycentre=rbind(barycentre,tapply(data_bis[,i], g, FUN = mean))
  }

  S=0
  for (i in 1:n_G){
    s=as.numeric(n_g[i])*sum((as.numeric(barycentre[,i])-as.numeric(G))^2)
    S=S+s
  }

  matrix_G=c()
  for (j in 1:row){
    matrix_G=rbind(matrix_G,G)
  }

  Inertie_total=sum((matrix_G-data_bis)^2)

  valeur_test=S/Inertie_total



  return(valeur_test)
}
