#' Title
#'
#' @param data a datafram or matrix with quantitative value
#' @param g a factor such that length(x)=nrow(data)
#'
#' @return rapport correlation
#' @export
#'
#' @examples
R2_multivariate=function(data,g){
  G=apply(data, MARGIN = 2, FUN = mean)
  n_g=tapply(data[,1],g,FUN = length)
  n_G=length(unique(g))
  columns=ncol(data)
  row=nrow(data)
  barycentre=c()
  for (i in 1:columns){
    barycentre=rbind(barycentre,tapply(data[,i], g, FUN = mean))
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

  Inertie_total=sum((matrix_G-data)^2)

  valeur_test=S/Inertie_total



  return(valeur_test)
}
