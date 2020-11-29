#' Title
#'
#' @return
#' @import  FactoMineR
#' @export
#'

#' @examples
acp_2_axes=function(X,i=1,j=2, rescale=FALSE){

  if (i==0|j==0|i>ncol(X) | j>ncol(X) ){
    return("the index must be larger than 0 and smaller than the number of variables")
    stop()
  }


#  if (data_type(X)=="quantitatives"){
#    X_bis=X
#  }

#  if (data_type(X)=="quantitative"){
#    X_bis=data.frame(X)
#  }

#  if (data_type(X)=='quantitative-qualitative'|data_type(X)=='qualitatives'){
#    X_bis=dummy_data(X,rescale)
#  }

#  if (data_type(X)=='qualitative'){
#    X_bis=dummy_cols(X, remove_first_dummy  = F)[,-1]
#  }


  acp=PCA(X,graph=FALSE)
  acp.ind=acp$ind
  acp.ind.cord=acp.ind$coord
  df=acp.ind.cord[,c(i,j)]
  dfbis=data.frame(df)
  PC1=round(acp$eig[,2][i])
  PC2=round(acp$eig[,2][j])
  colnames(dfbis)=c(paste("Dim",i,"---",PC1,"%"), paste("Dim",j,"---",PC2,"%"))
  return(dfbis)
}
