#' Verify the type of variables
#'
#' @param x a vector
#'
#' @return the type of x
#' @export
#'
#' @examples
type_variable=function(x){
  if (class(x)=='character'|length(unique(x))<7){
    type=('qualitative')
  } else{
    type=('quantitative')
  }
  return(type)
}



