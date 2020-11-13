
#' Title
#'
#' @param df a dataframe with numeric variables
#'
#' @return the correlation matrix using ggcorrplot vizualisation
#' @export
#' @import ggcorrplot
#'
#' @examples
#' viz_corr(mtcars)

viz_corr<- function(df){
  corr <- round(cor(df), 1)
  return(ggcorrplot(corr, hc.order = TRUE, type = "lower",lab = TRUE))
}
