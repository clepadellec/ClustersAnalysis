#' Title
#'
#' @param X dataframe or matrix
#'
#' @return the type of X: there are 5 possibilities: quantitative, quantitatives,
#' qualitative, qualitatives, quantitative-qualitative
#'
#' @export
#'
#' @examples
data_type=function(X){
#  quali_quanti=apply(X,MARGIN = 1, FUN = type_variable)
  quali_quanti=c()
  n=ncol(X)
  for (i in 1:n){
    quali_quanti=c(quali_quanti,type_variable(X[,i]))
  }
  tab=table(quali_quanti)
  name=names(tab)
  if (length(name)==2){
    type="quantitative-qualitative"
  } else{
    if (name=="quantitative"){
      if (tab[[1]]==1){
        type='quantitative'
      } else{
        type='quantitatives'
      }
    } else{
      if (tab[[1]]==1){
        type='qualitative'
      } else{
        type='qualitatives'
      }
    }
  }


  return(type)

}
