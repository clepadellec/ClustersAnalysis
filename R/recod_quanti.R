#' Title
#'
#' @param data
#' @param var_quanti
#' @param cutting
#'
#' @return
#' @export
#'
#' @examples
recod_quanti <- function(data, var_quanti,cutting){
  index <- which(colnames(data) == var_quanti)
  recod <- paste0(var_quanti,'_quali2')
  liste_labels <- NA
  for (i in 1:cutting){
    liste_labels[i] <- paste0(var_quanti,'.',i)
  }
  data$recod <- cut(data[,index], breaks = cutting, include.lowest = TRUE, labels = liste_labels)
  return(data)
}

data[record][,1] <- cut(data[,index], breaks = cutting, include.lowest = TRUE, labels = liste_labels)

data1 <- recod_quanti(data_0, 'Heureux', 4)

