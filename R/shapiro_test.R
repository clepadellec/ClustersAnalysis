#Fonction shapiro test
#' Title
#'
#' @param data a dataframe
#'
#' @return a dataframe with a summary of shapiro.test
#' @export
#'
#' @examples
#' #data=read.csv2("Données-R-1.csv", sep=";", row.names = 1)
#' #Data.quanti<- detection_quanti(data)
#' #shapiro_test(Data.quanti)
#'
shapiro_test <- function(data){
  res <- matrix(data = NA, nrow = (ncol(data)-1), ncol = 3)
  for (j in 1:(ncol(data)-1)){
    res[j,1] <- colnames(data[j])
    res[j,2] <- shapiro.test(data[,j])$p.value
    if (round(as.numeric(res[j,2]),5)<0.05){
      res[j,3]<-"significatif"
    }else{
      res[j,3]<-"non significatif"
    }
  }
  res <- as.data.frame(res)
  colnames(res)<-c("var.explicative","p.value.shapiro.test","interpretation")
  return(res)
}

#res = shapiro_test(X[[1]])
