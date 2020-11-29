#' Title
#'
#' @param X a matrix or dataframe with explanatory variables
#' @param y predicted labels for each row of X. It should be a factor
#' @param d the distance measure to be used and this must be "euclidean" or "L1"
#'
#' @return Silhouette Coefficient of each row
#' @export
#'
#' @examples
#'
silhouette_ind=function(X,y,rescale=FALSE,d='euclidean'){

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
    X_bis=dummy_cols(data.frame(X), remove_first_dummy  = F)[,-1]
  }



  matrice_distance=matrix_distance(X_bis,d)
  moyenne_distance=mean_distance(matrice_distance,y)
  sil=c()
  m=nrow(moyenne_distance)
  if (nlevels(y)==1){
    sil=rep(-1,m)
  } else{
    for (i in 1:m){
      if (sum(y==y[i])==1){
        sil[i]=-1
      } else{
        a=as.numeric(moyenne_distance[i,][as.character(y[i])])
        b=min(as.numeric(moyenne_distance[i,colnames(moyenne_distance)!=as.character(y[i])]))
        s=(b-a)/max(a,b)
        sil=c(sil,s)
      }
    }
  }

  return(sil)

}
