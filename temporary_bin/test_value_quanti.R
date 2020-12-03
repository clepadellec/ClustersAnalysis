#' Title
#'
#' @param data a dataframe or matrix with quantitatives values
#' @param g cluster identity
#' @param i i-th cluster
#' @return a dataframe with a summary of value test between the explanatory variables and and the i-th cluster
#' @export
#'
#' @examples
test.value=function(data,g, i=1){
  l=list()
  n_unique=length(unique(g))
  if (i>n_unique){
    return("the index must be smaller than the number of clusters")
    stop()
  }
  n=length(g)
  m=ncol(data)
  variance=apply(data,MARGIN = 2,FUN = function(x){return((n-1)/n*var(x))})
  moyenne_group=t(apply(data,MARGIN = 2, FUN=function(x){return(tapply(x,g,FUN=mean))}))
  moyenne=apply(data, MARGIN = 2,mean)
  len=tapply(data[,1],g, FUN = length)
  for ( j in 1:n_unique){
    VT=(moyenne_group[,j]-moyenne)/sqrt(((n-len[j])/(n-1)*(variance/len[j])))
    pvalue=rep(0,length(VT))
    for (k in 1:length(VT)){
      if (VT[k]<0){
        pvalue[k]=2*pnorm(VT[k])
      } else{
        pvalue[k]=2*(1-pnorm(VT[k]))
      }
    }
    df=data.frame(VT, pvalue)
    row.names(df)=colnames(data)
    colnames(df)[1]=as.character(unique(g)[j])
    df_sort=df[order(df$pvalue, decreasing = FALSE),]
    l[[j]]=df_sort
  }

  return(l[[i]])

}

