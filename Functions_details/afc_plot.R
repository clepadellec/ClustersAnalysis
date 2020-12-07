

#' Title
#'
#' @param contingence : a contingence table
#'
#' @return
#' @export
#' @import FactoMineR
#' @imort factoextra
#'
#' @examples
#'
afc_plot <- function(contingence){
  #Mise en place de l'AFC
  res.ca <- CA(contingence, graph = TRUE)
  #visualisation des valeurs propres
  eig.val <- get_eigenvalue(res.ca)
  #3 valeurs propres car min(n-1)(p-1)
  fviz_eig(res.ca, addlabels = TRUE, ylim = c(0,100))#graphique des valeurs propres avec la
  #Visualisation des profils lignes et colonnes dans un meme graphique
  #on garde que 1 axe, car coude + 73% d'inertie

  fviz_ca_biplot (res.ca, repel = TRUE) #argument repel pour eviter le chevauchement

}

