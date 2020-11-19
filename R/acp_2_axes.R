#' Title
#'
#' @return
#' @import  FactoMineR
#' @export
#'

#' @examples
acp_2_axes=function(X,i,j){
  if (i==0|j==0|i>ncol(X) | j>ncol(X) ){
    return("the index must be larger than 0 and smaller than the number of variables")
    stop()
  }

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
