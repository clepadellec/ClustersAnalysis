#' Title
#'
#' @param X a matrix or dataframe with explanatory variables
#' @param d the distance measure to be used and this must be "euclidean" or "L1"
#'
#' @return the distance matrix computed by using the euclidean distance or L1 distance between the rows of X.
#'
#'
#'
#' @examples
matrix_distance=function(X,d){
  m=nrow(X)
  n=ncol(X)
  distance=matrix(0,nrow = m, ncol=m)
  if (d=="euclidean"){
    for (i in 1:m){
      for (j in i:m){
        di=sqrt(sum((X[i,]-X[j,])^2))
        distance[i,j]=di
      }
    }
  } else{
    for (i in 1:m){
      for (j in i:m){
        di=sum(abs(X[i,]-X[j,]))
        distance[i,j]=di
      }
    }
  }


  dist=distance+t(distance)
  return(dist)
}
