#' Title
#'
#' @param g1 first cluster
#' @param g2 second cluster
#'
#' @return Rand index measure to compare the similarity of two clustering.
#' @export
#'
#' @examples
rand_index=function(g1,g2){
  if (length(g1)!= length(g2)){
    return("g1 and g2 must have the same length")
    stop()
  }
  a=0
  b=0
  c=0
  d=0
  n=length(g1)
  for (i in 1:(n-1)){
    for (j in (i+1):n){
      if ((g1[i]==g1[j]) & (g2[i]==g2[j])){
        a=a+1
      }
      if ((g1[i]!=g1[j]) & (g2[i]!=g2[j])){
        b=b+1
      }
    }
  }
  rand=(a+b)/((n*(n-1)/2))

  return(rand)
}
