#' Title
#'
#' @param X a matrix or dataframe with explanatory variables
#' @param d the distance measure to be used and this must be "euclidean" or "L1"
#'
#' @return the distance matrix computed by using the euclidean distance or L1 distance between the rows of X.
#'
#' @import rdist
#'
#' @export
#'
#' @examples
matrix_distance=function(X,d){
  dist=pdist(X,d)
  return(dist)
}
