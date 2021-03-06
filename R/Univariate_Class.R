##############################################################################
#                                 CONSTRUCTOR                                #-----------------------
##############################################################################



#' Constructor for the univariate object
#'
#' @param df your dataframe including explanatory variables and the group variable(target)
#' @param ind_group_class the indice of your group variable
#'
#' @return an object which is going to be use to analyze clustering
#' @export
#'
#' @examples Univariate_object(esoph,1)
#'
Univariate_object <- function(df,ind_group_class){
  ok <- is.data.frame(df)
  if (!ok){
    stop("Ce n'est pas un data frame")
  }


  #df_without_grp <- df[,-ind_group_class]
  instance <- list()

  #test si quanti ou quali
  ind.qual <- sapply(df, function(x) is.factor(x)| is.character(x))
  ind.quan <- sapply(df, function(x) is.numeric(x)| is.integer(x))

  #on recupere les indices associés
  instance$ind.qual <- ind.qual
  instance$ind.quan <- ind.quan
  #on supprime les lignes qui contiennent des NA
  df <- na.omit(df)
  #on passe la variable de groupe en character
  df[,ind_group_class] <- as.character(df[,ind_group_class])
  instance$df <- df
  instance$group <- ind_group_class
  instance$name_group <- colnames(df[ind_group_class])
  instance$lst_quali <- colnames(df[ ,ind.qual])
  instance$lst_quanti <- colnames(df[ ,ind.quan])

  #on regarde si il y a plusieurs vars explicatives ou non
  if ((length(ind.qual)+length(ind.quan))>1){
    instance$multiple_var=TRUE
  }else{
    instance$multiple_var=FALSE
  }

  return(instance)
}

##############################################################################
#              Contingency table and size effect                             #-----------------------
##############################################################################

###
#1#---
###

#' Function to see if some class are over-representend or not
#'
#' @param object your Univariate object
#' @param ind_var_exp the qualitative explanatory variable
#'
#' @return the vtest value for each class(group variable)/modality(explanatory variable)
#' @export
#'
#' @examples u_desc_size_effect((Univariate_object(esoph,1)),3)
#'
u_desc_size_effect <- function(object,ind_var_exp){

  #test si la variable est bien quali
  if (is.character(object$df[[ind_var_exp]])==FALSE & is.factor(object$df[[ind_var_exp]])==FALSE){
    return("Votre variable explicative n'est pas qualitative ! ")
    stop()
  }
  #on fait un tableau de contingence entre variable de groupe et variable expli quali
  tab_base <- table(object$df[[object$group]],object$df[[ind_var_exp]])
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



###
#2#---
###


#' See raws and columns profils, and a barplot
#'
#' @param object your Univariate object
#' @param ind_var_exp the indice of the explanatory variable (only qualitative)
#' @param interact if TRUE then an interactive vizualisation is generate with ggplotly, else there is just a classic plot
#' @return contingency table between group and explanatory variables and the rows and columns profils
#' @import questionr
#' @export
#'
#' @examples u_desc_profils((Univariate_object(esoph,1)),3)
u_desc_profils <- function(object,ind_var_exp,interact=TRUE){
  if (is.character(object$df[[ind_var_exp]])==FALSE & is.factor(object$df[[ind_var_exp]])==FALSE){
    return("Votre variable explicative n'est pas qualitative ! ")
    stop()
  }
  #mise en place du tableau de contingence
  contingence <- table(object$df[[object$group]],object$df[[ind_var_exp]])
  tab <- as.data.frame(contingence)
  colnames(tab)<- c("cluster","explanatory","Freq")
  #on affiche les resultats
  print("Tableau de contingence : ")
  print(contingence)
  print("Profils lignes : ")
  print(lprop(contingence, digits=1))
  print("Profils colonnes : ")
  print(cprop(contingence, digits=2))
  #on effectue un barplot pour visualiser la distribution des classes/Modalités
  p <-ggplot(tab, aes(fill = explanatory, y = Freq, x = cluster)) + geom_bar(position ="stack", stat = "identity")
  if (interact==TRUE){return(ggplotly(p))}else{return(p)}

}


###
#3#---
###

#' Create mosaic plot
#'
#' @param explanatory your explanatory variable
#' @param cluster
#'
#' @return
#' @export
#'
#' @examples
u_Mosaic_plot <- function(explanatory, cluster){

  #calcul des longueurs de vecteur
  levVar1 <- length(levels(explanatory))
  levVar2 <- length(levels(cluster))

  #creation contingence mais en proportion
  jointTable <- prop.table(table(explanatory, cluster))
  #transformation en df
  data <- as.data.frame(jointTable)
  #calcul des valeurs à afficher
  data$marginVar1 <- prop.table(table(explanatory))
  data$Clusters <- data$Freq / data$marginVar1
  data$Explanatory <- c(0, cumsum(data$marginVar1)[1:levVar1 -1]) +
    data$marginVar1 / 2

  #creation d'un mosaic plot à l'aide de barplot
  g<- ggplot(data, aes(Explanatory, Clusters)) +
    geom_bar(stat = "identity", aes(width = marginVar1, fill = cluster), col = "Grey") +
    geom_text(aes(label = as.character(explanatory), x = Explanatory, y = 1.1))


  return(g)
}

###
#4#---
###

#' Plot the size effect with a mosaic plot
#'
#' @param object your univariate object
#' @param ind_var_exp the indice of your explanatory variable(only qualitative)
#' @param interact if TRUE then an interactive vizualisation is generate with ggplotly, else there is just a classic plot
#' @return a mosaic plot which is the distribution of your explanatory variable by class
#'
#'
#' @rawNamespace import(plotly, except = last_plot)
#'
#' @import ggplot2
#' @export
#'
#' @examples u_plot_size_effect((Univariate_object(esoph,1)),2)
u_plot_size_effect<- function(object,name_var_exp, interact=TRUE){
  #on recup les données sur la var de groupe et la var explicative
  var_groupe <- object$name_group
  x=object$df[[name_var_exp]]
  y=object$df[[var_groupe]]

  #on créer le df correspondant
  df=data.frame("explanatory"=x, "cluster"=y)
  #on créer un objet ggplot qui correspond au mosaic plot
  p <- u_Mosaic_plot(df$explanatory,df$cluster)

  #si l'utilisiteur veut un graph interactif alors on utilise ggplotly sinon on retourne juste l'objet ggplot
  if (interact==TRUE){return(ggplotly(p))}else{return(p)}

}


##############################################################################
#                             Chisq test                                     #-----------------------
##############################################################################



#' Function to apply chisq test between your group variable and all your qualitatives variables
#'
#' @param object your Univariate object
#'
#' @return all the chisq.test value between your qualitatives variables and your group variable (with interpretation)
#' @import questionr
#' @export
#'
#' @examples u_chisq_test_all(Univariate_object(esoph,1))
u_chisq_test_all <- function(object){
  options(warn=-1)
  #Test s'il existe des vars quali
  if (length(object$ind.qual[object$ind.qual==TRUE])==1){
    return("Aucunes variables explicatives de votre dataframe n'est qualitative !")
    stop()
  }
  data= object$df[ ,object$ind.qual]
  #on recupère le nom des variables expli
  others_var <- colnames(data)[colnames(data)!=object$name_group]
  res <- matrix(data = NA, nrow = length(others_var), ncol = 5)

  #pour chaque variable explicative, on calcule le khi² et on interprete, si c'est significatif alors on calcule l'intensité avec le v de cramer
  for (i in 1:length(others_var)){
    res[i,1]<-object$name_group
    res[i,2]<-others_var[i]
    res[i,3]<-chisq.test(data[object$name_group][,1],data[others_var[i]][,1])$p.value
    if (round(as.numeric(res[i,3]),5)<0.05){
      res[i,4]<-"significatif"
      t<- table(data[object$name_group][,1],data[others_var[i]][,1])
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

##############################################################################
#                             Components analysis                            #-----------------------
##############################################################################


#' Vizualise modality and class on the same plan
#'
#' @param object your Univariate object
#' @param ind_var_exp the indice of the explanatory variable (only qualitative)
#'
#' @return a factorial projection between group variable and explanatory variable
#' @import FactoMineR
#' @import factoextra
#' @export
#'
#' @examples u_afc_plot((Univariate_object(esoph,1)),3)
u_afc_plot <- function(object,ind_var_exp){
  if (is.character(object$df[[ind_var_exp]])==FALSE & is.factor(object$df[[ind_var_exp]])==FALSE){
    return("Votre variable explicative n'est pas qualitative ! ")
    stop()
  }
  contingence <- table(object$df[[object$group]],object$df[[ind_var_exp]])
  #Mise en place de l'AFC
  res.ca <- CA(contingence, graph = FALSE)

  fviz_ca_biplot (res.ca, repel = TRUE) #argument repel pour eviter le chevauchement

}

##############################################################################
#             Means comparisons (using student test)                         #-----------------------
##############################################################################


###
#1#---
###

#' Shapiro test
#'
#' @param pop_a the first population to test
#' @param pop_b the second population to test
#'
#' @return TRUE if gaussian hypothesis is verified or FALSE if it's not verified
#' @export
#'
#' @examples
u_shapiro_test <- function(pop_a,pop_b){
  #on effectue le test de shapiro sur les deux pop
  pvalue_pop_a <- shapiro.test(pop_a)$p.value
  pvalue_pop_b <- shapiro.test(pop_b)$p.value

  #test si la p-value est >5% ou pop >30
  if((round(as.numeric(pvalue_pop_a),5)>0.05)|length(pop_a)>30){bool_a=TRUE}else{bool_a=FALSE}
  if((round(as.numeric(pvalue_pop_b),5)>0.05)|length(pop_b)>30){bool_b=TRUE}else{bool_b=FALSE}

  #si les deux ech sont gaussiens alors true sinon false
  if(bool_a==TRUE & bool_b==TRUE){hyp_gauss=TRUE}else{hyp_gauss=FALSE}
  return(hyp_gauss)
}


###
#2#---
###

#' Mean's comparison using student test
#'
#' @param object your Univariate object
#'
#' @return a table with the combination of each group X explanatory variable. For each combination, you can see if the fact to belong to a group as an influence on the explonatories variables (based on student test, means comparisons)
#' @export
#'
#' @examples u_ttest_all(Univariate_object(esoph,1))
u_ttest_all <- function(object){

  #pretraitement
  df <- na.omit(object$df)
  var_groupe <- object$name_group
  data= df[ ,object$ind.quan]
  #liste des groupes
  lst_grp <-unique(df[[var_groupe]])
  #données correpondant à la variable de groupe
  var_groupe <-df[[var_groupe]]

  data <- cbind(data,var_groupe)

  #creation de la matrice qu'on retournera
  res <- matrix(data = NA, nrow = length(lst_grp), ncol = length(object$ind.quan[object$ind.quan==TRUE]))
  lig=1
  #pour chaque classe
  for (i in lst_grp){
    col=1
    #on recode les modalités qui ne sont pas notre classe cible en "others"
    new_data=data
    new_data$var_groupe[new_data$var_groupe != i] <- "others"

    #on créer les deux populations à tester
    pop_a=new_data[new_data$var_groupe==i,]
    pop_b=new_data[new_data$var_groupe!=i,]
    gauss=TRUE

    #on teste la taille des echantillons pour savoir si on considère la normalité
    if(nrow(pop_a)<30|nrow(pop_b)<30){gauss=FALSE}
    for (j in 1:length(object$ind.quan[object$ind.quan==TRUE])){
      hyp_gauss=TRUE
      if (gauss==FALSE){
        #si hypothèse de normalité pas confirmée par la taille de l'ech alors on fait un test de shapiro
        hyp_gauss <- u_shapiro_test(pop_a[,j],pop_b[,j])
      }
      if (hyp_gauss==TRUE){
        #calcul et interpretation de la pvalue (t.test)
        pv=t.test(new_data[,j]~new_data$var_groupe)$p.value
        if(round(as.numeric(pv),5)<0.05){res[lig,col]="significatif"}else{res[lig,col]="non significatif"}

      }else{
        res[lig,col]=NA
      }
      col=col+1
    }
    lig=lig+1
  }
  rownames(res)<- lst_grp
  colnames(res)<- colnames(object$df[object$ind.quan])
  return(res)
}

##############################################################################
#                             Test Value                                     #-----------------------
##############################################################################



#' Calculate the test value
#'
#' @param object your Univariate object
#' @param i the number of your target group
#'
#' @return the explanatory variable which caracterize the most the class of your group variable you choose
#' @export
#'
#' @examples
u_test_value=function(object,i=1){
  var_groupe <- object$name_group
  data=data.frame(object$df[ ,object$ind.quan])

  g=object$df[[var_groupe]]
  l=list()
  n_unique=length(unique(g))

  if (i>n_unique){
    return("the index must be smaller than the number of clusters")
    stop()
  }
  n=length(g)
  m=ncol(data)
  variance=apply(data,MARGIN = 2,FUN = function(x){return((n-1)/n*var(x))})
  moyenne_group=t(apply(data,MARGIN = 2, FUN=function(x){return(tapply(x,g,FUN=mean))}))
  #print(moyenne_group)
  moyenne=apply(data, MARGIN = 2,mean)
  len=tapply(data[,1],g, FUN = length)
  for ( j in 1:n_unique){
    #calcul de la valeur test
    VT=(moyenne_group[,j]-moyenne)/sqrt(((n-len[j])/(n-1)*(variance/len[j])))
    pvalue=rep(0,length(VT))
    for (k in 1:length(VT)){
      #calcul de la pvalue associée à la valeur test
      if (VT[k]<0){
        pvalue[k]=2*pnorm(VT[k])
      } else{
        pvalue[k]=2*(1-pnorm(VT[k]))
      }
    }
    df=data.frame(VT, pvalue)
    row.names(df)=colnames(data)
    colnames(df)[1]=as.character(unique(g)[j])
    df_sort=df[order(df$pvalue, decreasing = FALSE),]
    l[[j]]=df_sort
  }

  return(l[[i]])

}

##############################################################################
#                             Correlation using Fisher                       #-----------------------
##############################################################################

###
#1#---
###

#' Calculate the correlation
#'
#' @param x a variable with quantitative value
#' @param g a factor such that length(x)=length(g)
#'
#' @return correlation value
#' @export
#'
#' @examples
u_eta2=function(x,g){

  #calcul des moyennes
  moyenne=tapply(x,g, FUN = mean)
  #calcul du nb d'indi
  individu=tapply(x,g,FUN = length)
  #calcul de la variance inter
  var_inter=sum(individu*((moyenne-mean(x))^2))
  #calcul de la variance totale
  var_total=sum((x-mean(x))^2)
  #calcul du rapport de corrélation
  eta=var_inter/var_total
  return(eta)
}


###
#2#---
###

#' Calculate correlation for each quantitatives variable and calculate the p-value using fisher
#'
#' @param object your Univariate object
#'
#' @return a dataframe with a summary of Fisher test between the target variable and others quanlitatives variables
#' @export
#'
#' @examples u_fisher_test_all(Univariate_object(iris,5))
u_fisher_test_all=function(object){
  var_groupe <- object$name_group
  data=object$df[ ,object$ind.quan]
  g=object$df[[var_groupe]]
  n=nrow(data)
  K=length(unique(g))
  #calculs des rapports de corrélation associés
  Eta2=apply(data,MARGIN = 2,FUN = function(x){return(u_eta2(x,g))})
  #calcul de la valeur test
  Test_value=(n-K)/(K-1)*(Eta2)/(1-Eta2)
  #puis de la p-value
  p_value=1-pf(Test_value,K-1,n-K)
  df=data.frame('Eta2'=Eta2, 'Test_value'=Test_value, 'p_value'=p_value)
  return(df)
}


