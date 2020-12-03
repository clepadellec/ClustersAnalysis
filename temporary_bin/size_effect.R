
#' Title
#'
#' @param df the dataframe
#' @param var_expli
#' @param target_grp
#'
#' @return a description (distribution) of size effect between a group choose by the user and an other variable(for each modality)
#' @export
#'
#' @examples
#'
#'
desc_size_effect <- function(df,target_grp,var_expli){

  #on fait un tableau de contingence entre variable de groupe et variable expli quali
  tab_base <- table(df[[target_grp]],df[[var_expli]])
  #on ajoute les totaux
  tab <- addmargins(tab_base)
  #on affiche le tableau
  print(tab)
  vtest <- tab_base
  #pour chaque paire de modalités on calcule le vtest
  for (i in 1:nrow(tab_base)){
    for (j in 1:ncol(tab_base)){
      prop_l_g <- tab[i,j]/tab[i,ncol(tab_base)+1]
      prop_l <- tab[nrow(tab_base)+1,j]/tab[nrow(tab_base)+1,ncol(tab_base)+1]
      r=sqrt((tab[nrow(tab_base)+1,ncol(tab_base)+1]-tab[i,ncol(tab_base)+1])/(tab[nrow(tab_base)+1,ncol(tab_base)+1]-1))
      vtest[i,j]<-sqrt(tab[i,ncol(tab_base)+1])*((prop_l_g-prop_l)/(r*prop_l*(1-prop_l)))
    }
  }
  return(vtest)
}


