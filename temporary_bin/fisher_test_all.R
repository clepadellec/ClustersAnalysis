#' Title
#'
#' @param data a dataframe or matrix
#' @param g cluster identity
#'
#' @return a dataframe with a summary of Fisher test between the target variable and others quanlitatives variables
#' @export
#'
#' @examples
fisher_test_all=function(data, g){
  n=nrow(data)
  K=length(unique(g))
  Eta2=apply(data,MARGIN = 2,FUN = function(x){return(eta2(x,g))})
  Test_value=(n-K)/(K-1)*(Eta2)/(1-Eta2)
  p_value=1-pf(Test_value,K-1,n-K)
  df=data.frame('Eta2'=Eta2, 'Test_value'=Test_value, 'p_value'=p_value)
  return(df)
}
