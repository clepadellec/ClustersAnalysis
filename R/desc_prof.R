
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
desc_profils <- function(data,name_var_group,name_var_comp){
  contigence<-table(data[name_var_group][,1], data[name_var_comp][,1])
  print("Tableau de contingence : \n")
  print(contingence)
  print("Profils lignes : \n")
  lprop(contingence_h_r, digits=1)
  print("Profils colonnes : \n")
  cprop(contingence_h_r, digits=2)
}
