#' Title
#'
#' @param X a numeric symmetric matrix containing the pairwise distance between the rows of a data frame
#' @param y a factor such that length(X)=length(y)
#'
#' @return a data frame containing the average distance of each row to all observations of each cluster C
#'
#'
#' @examples
mean_distance=function(X,y){

  m=nrow(X)
  distance=c()
  for (i in 1:m){
    distance=rbind(distance,tapply(X[i,-i],y[-i], mean))
  }

  return(distance)

}
