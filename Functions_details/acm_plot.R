
#' Title
#'
#' @param df a dataframe with qualitatives features
#' @param dims dimensions to print
#' @param name_ind rownames to apply
#' @param qtsup if users want to add a quantitative features to acm (could be a number or vector of numbers)
#' @import FactoMineR
#' @import factoextra
#' @import corrplot
#' @return corrplot with the contributions of features for each dimensions and a biplot to vizualize acm
#' @export
#'
#' @examples
#'
acm_plot <- function(df,dims,name_ind=0, qtsup=NULL){

  #si name_ind est différent de zero, alors on affecte les rownames au df
  if (name_ind != 0){
    rownames(df)=name_ind
  }

  #creation de l'acm sur le dataframe,
  res.mca <- MCA (df, graph = FALSE, quanti.sup=qtsup)

  #on recupère les résultats pour les individus
  ind <- get_mca_ind (res.mca)
  #et pour les variables
  var <- get_mca_var(res.mca)
  #on affiche un corrplot pour visualiser les cos carré
  corrplot(var$cos2, is.corr=FALSE)

  #en fonction de s'il y a des rownames ou non on affiche un graph différent
  if (name_ind != 0){
    fviz_mca_biplot(res.mca, select.ind = list(cos2 = 75),axes=dims,
                    repel = TRUE,geom=c("text", "text"))
  }else{
    fviz_mca_biplot(res.mca, select.ind = list(cos2 = 75),axes=dims,
                    repel = TRUE,geom=c("point", "text"))
  }





}

