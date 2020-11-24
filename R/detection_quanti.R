#' Title
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples

detection_quanti <- function(data){
  # Repère var quanti
  type_var <- data %>% head %>% collect %>% lapply(class) %>% unlist
  # Repère index var quanti
  var_quanti <- c(which(type_var == "numeric" | type_var == "integer"))
  # Supprime var quanti
  data <- data[,-var_quanti]

  return(data)
}

detection_quanti(data_0)

library(dplyr)




