### Groupes = heureux_quali ###

#recupérer toutes var quanti
data_quanti = detection_quanti(data_0)
data_quanti_tot = merge(data_quanti, data_0$heureux_quali)

#spliter en fonction des groupes
X <- split(data_quanti_tot[1:29,], data_quanti_tot[1:29,]$y)

#regarder taille
for (i in 1:length(X)){
  if (nrow(X[[i]]) < 30){
    res = shapiro_test(X[[i]])
    #garde seulement les variables significatives
    ind = which(res$interpretation == "significatif")
    var.exp = c(res$var.explicative[ind])
  }
}


#si pop < 30 -> shapiro

#sinon direct anova



