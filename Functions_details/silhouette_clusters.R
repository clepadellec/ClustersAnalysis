#' Title
#'
#' @return
#' @export
#'
#' @examples
#'
#'
sil_plot=function(X,y,d){
  sil=silhouette_ind(X,y,d)
  data=data.frame("sil"=sil,"labels"=y)

}
