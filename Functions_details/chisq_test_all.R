

#' Title
#'
#' @param name_var_group
#' @param data dataframe with qualitative columns (use extract_ql(dataframe) to extract qualitative variables)
#' @param name_var_group string with represent the name of target column
#' @return a dataframe with a summary of chisq.test between the target variable and others qualitatives variables
#' @export
#'
#' @examples
#' #(change example)
#' #data=read.csv2("Données-R-1.csv", sep=";", row.names = 1)
#' #Data.qual<- extract_ql(data)
#' #chisq_test_all("Continent",Data.qual)

chisq_test_all <- function(name_var_group,data){
  options(warn=-1)
  #on recupère le nom des variables expli
  others_var <- colnames(data)[colnames(data)!=name_var_group]
  res <- matrix(data = NA, nrow = length(others_var), ncol = 5)

  #pour chaque variable explicative, on calcule le khi² et on interprete, si c'est significatif alors on calcule l'intensité avec le v de cramer
  for (i in 1:length(others_var)){
    res[i,1]<-name_var_group
    res[i,2]<-others_var[i]
    res[i,3]<-chisq.test(data[name_var_group][,1],data[others_var[i]][,1])$p.value
    if (round(as.numeric(res[i,3]),5)<0.05){
      res[i,4]<-"significatif"
      t<- table(data[name_var_group][,1],data[others_var[i]][,1])
      v<- cramer.v(t)
      res[i,5]<-v
    }else{
      res[i,4]<-"non significatif"
      res[i,5]<-NA
    }

  }
  res <- as.data.frame(res)
  colnames(res)<-c("var.groupement","var.explicative","p.value.chisq.test","interpretation","intensité(v cramer)")
  return(res)
}
