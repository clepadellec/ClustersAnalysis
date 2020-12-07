#fonction "extract_ql"
#' Title
#'
#' @param data a dataframe
#'
#' @return the dataframe with only qualitative data
#' @export
#'
#' @examples
extract_ql <- function(data){
  ind.qual <- sapply(data, function(x) is.factor(x)| is.character(x))
  Data.qual <- data[ ,ind.qual]
  return(Data.qual)
}

