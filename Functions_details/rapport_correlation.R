#' Title
#'
#' @param x a variable with quantitative value
#' @param g a factor such that length(x)=length(g)
#'
#' @return rapport correlation
#' @export
#'
#' @examples
eta2=function(x,g){
  moyenne=tapply(x,g, FUN = mean)
  individu=tapply(x,g,FUN = length)
  var_inter=sum(individu*((moyenne-mean(x))^2))
  var_total=sum((x-mean(x))^2)
  eta=var_inter/var_total
  return(eta)
}
