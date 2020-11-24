#' Title
#'
#' @param X dataframe or matrix
#' @param y cluster identity by using another algorithm or anothers explanatory variables
#'
#' @return rand index between the result of kmean and y
#' @export
#'
#' @examples
kmean_rand_index=function(X,y){
  n=length(unique(y))
  X_cr=scale(X,center = T,scale = T)
  n_means=kmeans(X_cr,centers = n,nstart = 5)

  rand=rand_index(as.numeric(n_means$cluster),y)
  return(rand)

}
