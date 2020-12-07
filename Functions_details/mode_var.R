

#' Title
#'
#' @param x a column with qualitative values or discrete values
#'
#' @return the value that appears the most (mode)
#' @export
#'
#' @examples
mode_var <- function(x){
  return(names(sort(table(x),decreasing=TRUE))[1])
}
