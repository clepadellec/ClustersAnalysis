#' Title
#'
#' @return
#' @export
#'
#' @examples
kmean_clustering_plot=function(X,y,i,j){

  if (class(y)!="factor"){
    return("y must be a factor")
    stop()
  }

  if (i==0|j==0|i>ncol(X) | j>ncol(X) ){
    return("the index must be larger than 0 and smaller than the number of variables")
    stop()
  }

  acp=acp_2_axes(X,i,j)
  a=colnames(acp)[1]
  b=colnames(acp)[2]
  percent1=as.numeric(substr(a,11,12))
  percent2=as.numeric(substr(b,11,12))
  cluster=y
  colnames(acp)=c("Dimi", "Dimj")


  n=length(unique(y))
  X_cr=scale(X,center = T,scale = T)
  n_means=kmeans(X_cr,centers = n,nstart = 5)
  cluster_kmean=n_means$cluster

  g= ggplot(acp, aes(Dimi,Dimj, color =cluster_kmean, shape =cluster)) +
    geom_point(size=3) +   labs(x = paste("Dim", i,'---', percent1, "%"), y = paste("Dim", j,'---', percent2, "%"))+
    theme(text = element_text(family = "serif", size=14), title = element_text(color = "#8b0000"))

  return(g)

}
