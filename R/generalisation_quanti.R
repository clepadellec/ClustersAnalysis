### Groupes = Régime_simp ###

# detection_quanti <- function(data){
#   # Repère var quanti
#   type_var <- data %>% head %>% collect %>% lapply(class) %>% unlist
#   # Repère index var quanti
#   var_quanti <- c(which(type_var == "numeric" | type_var == "integer"))
#   # Supprime var quanti
#   data <- data[,var_quanti]
#
#   return(data)
# }
#
#
# shapiro_test <- function(data){
#   res <- matrix(data = NA, nrow = (ncol(data)-1), ncol = 3)
#   for (j in 1:(ncol(data)-1)){
#     res[j,1] <- colnames(data[j])
#     res[j,2] <- shapiro.test(data[,j])$p.value
#     if (round(as.numeric(res[j,2]),5)<0.05){
#       res[j,3]<-"significatif"
#     }else{
#       res[j,3]<-"non significatif"
#     }
#   }
#   res <- as.data.frame(res)
#   colnames(res)<-c("var.explicative","p.value.shapiro.test","interpretation")
#   return(res)
# }
#
# data=read.csv2("Données-R-1.csv", sep=";", row.names = 1)
#
# #recupérer toutes var quanti
# data_quanti = detection_quanti(data)
# # data_quanti_tot = merge(data_quanti, data_0$heureux_quali)
# #
# # #spliter en fonction des groupes
# # X <- split(data_quanti_tot[1:29,], data_quanti_tot[1:29,]$y)
# X <- split(data_quanti, data$Régime_simp)
# #
# # #regarder taille
# for (i in 1:length(X)){
#   if (nrow(X[[i]]) < 30){
#     res = shapiro_test(X[[i]])
#    #garde seulement les variables significatives
#     ind = which(res$interpretation == "significatif")
#     var.exp = c(res$var.explicative[ind])
#   }
# }
#
# #cherche noms colonnes
# ind = which(colnames(data) == colnames(data_quanti))
# data[ind]
#
# #anova
# a<-aov(data[ind] ~ data$Regime_poli)
# b <- summary(a)
# names(b[2])


#' Title
#'
#' @param df
#' @param var_groupe
#'
#' @return
#' @export
#'
#' @examples
aov_all <- function(df,var_groupe){
    type_var <- df %>% head %>% collect %>% lapply(class) %>% unlist
    # Repère index var quanti
    var_quanti <- c(which(type_var == "numeric" | type_var == "integer"))
    # Supprime var quanti
    data <- df[,c(var_quanti)]
    var_groupe <-df[[var_groupe]]
    data <- cbind(data,var_groupe)
    lst_grp<-unique(var_groupe)
    #print(data$var_groupe)
    for (i in lst_grp){
      new_data=data
      new_data$var_groupe[new_data$var_groupe != i] <- "others"
      for (j in 1:length(var_quanti)){
        print("-------------------------------------------")
        print(paste("i : ",i))
        print(paste("j : ",j))
        print(t.test(new_data[,j]~new_data$var_groupe)$p.value)
      }

    }
    #return(data)
}

#ttest_all(data_0,'Region')

