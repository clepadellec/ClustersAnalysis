#' Title
#'
#' @return
#'
#' @import  ggplot2
#' @import FactoMineR
#' @export
#'
#' @examples
sil_pca_plot=function(X,y,i=1,j=2, rescale=FALSE, d="euclidean"){

  if (class(y)!="factor"){
    return("y must be a factor")
    stop()
  }

  if (i==0|j==0|i>ncol(X) | j>ncol(X) ){
    return("the index must be larger than 0 and smaller than the number of variables")
    stop()
  }


  if (data_type(X)=="quantitatives"){
    X_bis=X
  }

  if (data_type(X)=="quantitative"){
    X_bis=data.frame(X)
  }

  if (data_type(X)=='quantitative-qualitative'|data_type(X)=='qualitatives'){
    X_bis=dummy_data(X,rescale)
  }

  if (data_type(X)=='qualitative'){
    X_bis=dummy_cols(X, remove_first_dummy  = F)[,-1]
  }



  acp=acp_2_axes(X_bis,i,j)
  sil=silhouette_ind(X,y,rescale,d)
  a=colnames(acp)[1]
  b=colnames(acp)[2]
  percent1=as.numeric(substr(a,11,12))
  percent2=as.numeric(substr(b,11,12))
  cluster=y
  colnames(acp)=c("Dimi", "Dimj")
  g= ggplot(acp, aes(Dimi,Dimj, color =sil, shape =cluster)) +
    geom_point(size=3) +   labs(x = paste("Dim", i,'---', percent1, "%"), y = paste("Dim", j,'---', percent2, "%"))+
    theme(text = element_text(family = "serif", size=14), title = element_text(color = "#8b0000"))

  return(g)

}


