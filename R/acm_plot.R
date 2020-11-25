
#' Title
#'
#' @param df a dataframe with qualitatives features
#' @param dims dimensions to print
#' @param name_ind rownames to apply
#' @import FactoMineR
#' @imort factoextra
#' @return corrplot with the contributions of features for each dimensions and a biplot to vizualize acm
#' @export
#'
#' @examples
#'
acm_plot <- function(df,dims,name_ind=0){
  if (name_ind != 0){
    rownames(df)=name_ind
  }
  res.mca <- MCA (df, graph = FALSE)
  ind <- get_mca_ind (res.mca)
  var <- get_mca_var(res.mca)
  corrplot(var$cos2, is.corr=FALSE)
  if (name_ind != 0){
    print("cc")
    fviz_mca_biplot(res.mca, select.ind = list(cos2 = 75),axes=dims,
                    repel = TRUE,geom=c("text", "text"))
  }else{
    print("dd")
    fviz_mca_biplot(res.mca, select.ind = list(cos2 = 75),axes=dims,
                    repel = TRUE,geom=c("point", "text"))
  }





}

