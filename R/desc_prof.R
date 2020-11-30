
#' desc_prof
#'
#' @param data
#' @param name_var_group the target variable (group variable)
#' @param name_var_comp the other qualitative variable
#'
#' @return contingence table, and profils
#' @export
#' @import questionr
#' @examples
#'#desc_profils(data,"heureux_quali","Region")
desc_profils <- function(data,name_var_group,name_var_comp){
  #mise en place du tableau de contingence
  contingence<-table(data[name_var_group][,1], data[name_var_comp][,1])
  #on affiche les resultats
  print("Tableau de contingence : ")
  print(contingence)
  print("Profils lignes : ")
  print(lprop(contingence, digits=1))
  print("Profils colonnes : ")
  print(cprop(contingence, digits=2))
}

