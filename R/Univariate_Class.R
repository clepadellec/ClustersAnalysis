#' Title
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
  ind.qual <- sapply(df, function(x) is.factor(x)| is.character(x))
  ind.quan <- sapply(df, function(x) is.numeric(x)| is.integer(x))
  instance$ind.qual <- ind.qual
  instance$ind.quan <- ind.quan
  df <- na.omit(df)
  df[,ind_group_class] <- as.character(df[,ind_group_class])
  instance$df <- df
  instance$group <- ind_group_class
  instance$name_group <- colnames(df[ind_group_class])
  instance$lst_quali <- colnames(df[ ,ind.qual])
  instance$lst_quanti <- colnames(df[ ,ind.quan])

  if ((length(ind.qual)+length(ind.quan))>1){
    instance$multiple_var=TRUE
  }else{
    instance$multiple_var=FALSE
  }

  return(instance)
}


#' Title
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

#' Title
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
  if (length(object$ind.quan[object$ind.qual==TRUE]==1)){
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

#' Title
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

#' Title
#'
#' @param object your Univariate object
#' @param ind_var_exp the indice of the explanatory variable (only qualitative)
#'
#' @return contingency table between group and explanatory variables and the rows and columns profils
#' @import questionr
#' @export
#'
#' @examples u_desc_profils((Univariate_object(esoph,1)),3)
u_desc_profils <- function(object,ind_var_exp){
  if (is.character(object$df[[ind_var_exp]])==FALSE & is.factor(object$df[[ind_var_exp]])==FALSE){
    return("Votre variable explicative n'est pas qualitative ! ")
    stop()
  }
  #mise en place du tableau de contingence
  contingence <- table(object$df[[object$group]],object$df[[ind_var_exp]])
  #on affiche les resultats
  print("Tableau de contingence : ")
  print(contingence)
  print("Profils lignes : ")
  print(lprop(contingence, digits=1))
  print("Profils colonnes : ")
  print(cprop(contingence, digits=2))
}

#' Title
#'
#' @param object your univariate object
#' @param ind_var_exp the indice of your explanatory variable(only qualitative)
#'
#' @return a mosaic plot which is the distribution of your explanatory variable by class
#'
#'
#' @rawNamespace import(plotly, except = last_plot)
#' @import ggplot2
#' @import ggmosaic
#'
#' @export
#'
#' @examples #u_plot_size_effect((Univariate_object(infert,1)),2)
u_plot_size_effect<- function(object,ind_var_exp){
  var_groupe <- object$name_group
  x=object$df[,ind_var_exp]
  y=object$df[[var_groupe]]
  df=data.frame("explanatory"=x, "cluster"=y)
  p <- ggplot(data = df) +
    geom_mosaic(aes(x = product(explanatory), fill=cluster), na.rm=TRUE) +
    labs(x = colnames(object$df[2]),y=colnames(object$df[1]), title='Distribution between cluster and explanatory')+
    theme(axis.text.x = element_blank(), axis.text.y = element_blank())

  return(ggplotly(p))
}

#' Title
#'
#' @param pop_a the first population to test
#' @param pop_b the second population to test
#'
#' @return TRUE if gaussian hypothesis is verified or FALSE if it's not verified
#' @export
#'
#' @examples
u_shapiro_test <- function(pop_a,pop_b){
  pvalue_pop_a <- shapiro.test(pop_a)$p.value
  pvalue_pop_b <- shapiro.test(pop_b)$p.value
  if((round(as.numeric(pvalue_pop_a),5)>0.05)|length(pop_a)>30){bool_a=TRUE}else{bool_a=FALSE}
  if((round(as.numeric(pvalue_pop_b),5)>0.05)|length(pop_b)>30){bool_b=TRUE}else{bool_b=FALSE}
  if(bool_a==TRUE & bool_b==TRUE){hyp_gauss=TRUE}else{hyp_gauss=FALSE}
  return(hyp_gauss)
}

#' Title
#'
#' @param object your Univariate object
#'
#' @return a table with the combination of each group X explanatory variable. For each combination, you can see if the fact to belong to a group as an influence on the explonatories variables (based on student test, means comparisons)
#' @export
#'
#' @examples u_ttest_all(Univariate_object(esoph,1))
u_ttest_all <- function(object){
  df <- na.omit(object$df)
  var_groupe <- object$name_group
  data= df[ ,object$ind.quan]
  lst_grp <-unique(df[[var_groupe]])
  var_groupe <-df[[var_groupe]]

  data <- cbind(data,var_groupe)
  res <- matrix(data = NA, nrow = length(lst_grp), ncol = length(object$ind.quan[object$ind.quan==TRUE]))
  lig=1
  for (i in lst_grp){
    col=1
    new_data=data
    new_data$var_groupe[new_data$var_groupe != i] <- "others"
    pop_a=new_data[new_data$var_groupe==i,]
    pop_b=new_data[new_data$var_groupe!=i,]
    gauss=TRUE
    if(nrow(pop_a)<30|nrow(pop_b)<30){gauss=FALSE}
    for (j in 1:length(object$ind.quan[object$ind.quan==TRUE])){
      hyp_gauss=TRUE
      if (gauss==FALSE){
        hyp_gauss <- u_shapiro_test(pop_a[,j],pop_b[,j])
      }
      if (hyp_gauss==TRUE){
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


#' Title
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
    VT=(moyenne_group[,j]-moyenne)/sqrt(((n-len[j])/(n-1)*(variance/len[j])))
    pvalue=rep(0,length(VT))
    for (k in 1:length(VT)){
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

#' Title
#'
#' @param x variable to test
#'
#' @return variable type
#' @export
#'
#' @examples
u_type_variable=function(x){
  if (class(x)=='character'|length(unique(x))<7){
    type=('qualitative')
  } else{
    type=('quantitative')
  }
  return(type)
}

#' Title
#'
#' @param X a dataframe
#'
#' @return the variables types
#' @export
#'
#' @examples
u_data_type=function(X){
  quali_quanti=sapply(X, FUN = u_type_variable)
  #  quali_quanti=c()
  #  n=ncol(X)
  #  for (i in 1:n){
  #    quali_quanti=c(quali_quanti,type_variable(X[,i]))
  #  }
  tab=table(quali_quanti)
  name=names(tab)
  if (length(name)==2){
    type="quantitative-qualitative"
  } else{
    if (name=="quantitative"){
      if (tab[[1]]==1){
        type='quantitative'
      } else{
        type='quantitatives'
      }
    } else{
      if (tab[[1]]==1){
        type='qualitative'
      } else{
        type='qualitatives'
      }
    }
  }


  return(type)

}

#' Title
#'
#' @param data a datafram
#' @param rescale a boolean
#'
#' @return data encoding with rescale or not
#'
#' @import fastDummies
#' @export
#'
#' @examples
u_dummy_data=function(data, rescale=FALSE){

  dataf=data

  col_names=colnames(data)
  t=sapply(data, u_type_variable)
  variable_qualitative=colnames(data)[t=="qualitative"]
  variable_quantitative=colnames(data)[t=="quantitative"]
  if (length(variable_qualitative)==0){
    return("There is no qualitative variable in this data")
    break
  } else{
    dataf= dummy_cols(data, select_columns = variable_qualitative, remove_selected_columns = TRUE)
  }


  if (rescale==TRUE){
    if (length(variable_quantitative)==0){
      return("There is no quantitative variable in this data")
      break
    } else{
      for (i in variable_quantitative){
        dataf[i]=scale(dataf[i], center = T, scale = T)
      }
    }
  }
  return(dataf)
}
#' Title
#'
#' @param X a matrix or dataframe with explanatory variables
#' @param d the distance measure to be used and this must be "euclidean" or "L1"
#'
#' @return the distance matrix computed by using the euclidean distance or L1 distance between the rows of X.
#'
#' @import rdist
#'
#' @export
#'
#' @examples
u_matrix_distance=function(X,d){
  dist=pdist(X,d)
  return(dist)
}

#' Title
#'
#' @param X a numeric symmetric matrix containing the pairwise distance between the rows of a data frame
#' @param y a factor such that length(X)=length(y)
#'
#' @return a data frame containing the average distance of each row to all observations of each cluster C
#' @export
#'
#' @examples
u_mean_distance=function(X,y){

  m=nrow(X)
  distance=c()
  for (i in 1:m){
    distance=rbind(distance,tapply(X[i,-i],y[-i], mean))
  }

  return(distance)

}

#' Title
#'
#' @param object your Univariate object
#' @param rescale rescale or not
#' @param d distance type
#'
#' @return Silhouette Coefficient of each row
#' @export
#'
#' @examples u_silhouette_ind(Univariate_object(iris,5))
u_silhouette_ind=function(object,rescale=FALSE,d='euclidean'){
  indice= object$group

  X=object$df[,-indice]

  var_groupe <- object$name_group

  y=object$df[[var_groupe]]

  if (u_data_type(X)=="quantitatives"){
    X_bis=X
  }

  if (u_data_type(X)=="quantitative"){
    X_bis=data.frame(X)
  }

  if (u_data_type(X)=='quantitative-qualitative'|u_data_type(X)=='qualitatives'){
    X_bis=u_dummy_data(X,rescale)
  }

  if (u_data_type(X)=='qualitative'){
    X_bis=dummy_cols(data.frame(X), remove_first_dummy  = F)[,-1]
  }



  matrice_distance=u_matrix_distance(X_bis,d)
  moyenne_distance=u_mean_distance(matrice_distance,y)
  sil=c()
  m=nrow(moyenne_distance)
  if (nlevels(y)==1){
    sil=rep(-1,m)
  } else{
    for (i in 1:m){
      if (sum(y==y[i])==1){
        sil[i]=-1
      } else{
        a=as.numeric(moyenne_distance[i,][as.character(y[i])])
        b=min(as.numeric(moyenne_distance[i,colnames(moyenne_distance)!=as.character(y[i])]))
        s=(b-a)/max(a,b)
        sil=c(sil,s)
      }
    }
  }

  return(sil)

}

#' Title
#'
#' @param object your Univariate object
#' @param rescale rescale or not
#' @param d the distance measure to be used and this must be "euclidean" or "L1"
#'
#' @return the silhouette plot
#' @rawNamespace import(plotly, except = last_plot)
#' @import ggplot2
#' @export
#'
#' @examples u_silhouette_plot(Univariate_object(iris,5))
u_silhouette_plot=function(object, rescale=FALSE, d="euclidean"){
  var_groupe <- object$name_group
  sil=u_silhouette_ind(object,rescale,d)
  y=object$df[[var_groupe]]
  df=data.frame("silhouette"=sil, "cluster"=y)

  # data frame contains the mean silhouette coefficient of clusters

  t=tapply(df[,"silhouette"], df[,"cluster"],mean)
  df_bis=data.frame("silhouette"=as.numeric(t), "cluster"=names(t))
  # plot
  g=ggplot(df, aes(cluster,silhouette,color=cluster))+geom_point(size=1.5)+ geom_point(data = df_bis, shape=15,size=4)+
    theme(text = element_text(family = "serif", size=14), title = element_text(color = "#8b0000"))+
    labs(title="Silhouette coefficient ") +ylim(-1,1)

 return(ggplotly(g))

}

#' Title
#'
#' @param x a variable with quantitative value
#' @param g a factor such that length(x)=length(g)
#'
#' @return correlation value
#' @export
#'
#' @examples
u_eta2=function(x,g){
  moyenne=tapply(x,g, FUN = mean)
  individu=tapply(x,g,FUN = length)
  var_inter=sum(individu*((moyenne-mean(x))^2))
  var_total=sum((x-mean(x))^2)
  eta=var_inter/var_total
  return(eta)
}

#' Title
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
  Eta2=apply(data,MARGIN = 2,FUN = function(x){return(u_eta2(x,g))})
  Test_value=(n-K)/(K-1)*(Eta2)/(1-Eta2)
  p_value=1-pf(Test_value,K-1,n-K)
  df=data.frame('Eta2'=Eta2, 'Test_value'=Test_value, 'p_value'=p_value)
  return(df)
}

#' Title
#'
#' @param g1 first cluster
#' @param g2 second cluster
#'
#' @return Rand index measure to compare the similarity of two clustering.
#' @export
#'
#' @examples
u_rand_index=function(g1,g2){
  if (length(g1)!= length(g2)){
    return("g1 and g2 must have the same length")
    stop()
  }
  a=0
  b=0
  c=0
  d=0
  n=length(g1)
  for (i in 1:(n-1)){
    for (j in (i+1):n){
      if ((g1[i]==g1[j]) & (g2[i]==g2[j])){
        a=a+1
      }
      if ((g1[i]!=g1[j]) & (g2[i]!=g2[j])){
        b=b+1
      }
    }
  }
  rand=(a+b)/((n*(n-1)/2))

  return(rand)
}



#' Title
#'
#' @param object your Univariate object
#'
#' @return rand index between the result of kmean and your group variable
#' @export
#'
#' @examples u_kmean_rand_index(Univariate_object(iris,5))
u_kmean_rand_index=function(object){
  indice= object$group

  X=object$df[ ,object$ind.quan]
  var_groupe <- object$name_group

  y=object$df[[var_groupe]]
  n=length(unique(y))
  X_cr=scale(X,center = T,scale = T)
  n_means=kmeans(X_cr,centers = n,nstart = 5)

  rand=u_rand_index(n_means$cluster,y)
  return(rand)

}
#' Title
#'
#' @return dataframe with ACP results for 2 axes
#' @import FactoMineR
#' @export
#'

#' @param X a dataframe
#'
#' @param i first dim
#' @param j second dim
#' @param rescale a boolean
#'
#' @examples
u_acp_2_axes=function(X,i=1,j=2, rescale=FALSE){

  if (i==0|j==0|i>ncol(X) | j>ncol(X) ){
    return("the index must be larger than 0 and smaller than the number of variables")
    stop()
  }



  acp=PCA(X,graph=FALSE)
  acp.ind=acp$ind
  acp.ind.cord=acp.ind$coord
  df=acp.ind.cord[,c(i,j)]
  dfbis=data.frame(df)
  PC1=round(acp$eig[,2][i])
  PC2=round(acp$eig[,2][j])
  colnames(dfbis)=c(paste("Dim",i,"---",PC1,"%"), paste("Dim",j,"---",PC2,"%"))
  return(dfbis)
}

#' Title
#'
#' @param object
#' @param i
#' @param j
#' @param rescale
#' @param d
#'
#' @return
#' @rawNamespace import(plotly, except = last_plot)
#' @import ggplot2
#'
#' @import FactoMineR
#' @export
#'
#' @examples u_sil_pca_plot(Univariate_object(iris,5))
u_sil_pca_plot=function(object,i=1,j=2, rescale=FALSE, d="euclidean"){
  indice= object$group

  X=object$df[,-indice]

  var_groupe <- object$name_group

  y=object$df[[var_groupe]]
  if (class(y)!="factor" & class(y)!="character"){
    return("y must be a factor or character")
    stop()
  }

  if (i==0|j==0|i>ncol(X) | j>ncol(X) ){
    return("the index must be larger than 0 and smaller than the number of variables")
    stop()
  }


  if (u_data_type(X)=="quantitatives"){
    X_bis=X
  }

  if (u_data_type(X)=="quantitative"){
    X_bis=data.frame(X)
  }

  if (u_data_type(X)=='quantitative-qualitative'|u_data_type(X)=='qualitatives'){
    X_bis=u_dummy_data(X,rescale)
  }

  if (u_data_type(X)=='qualitative'){
    X_bis=dummy_cols(X, remove_first_dummy  = F)[,-1]
  }



  acp=u_acp_2_axes(X_bis,i,j)
  sil=u_silhouette_ind(object,rescale,d)
  a=colnames(acp)[1]
  b=colnames(acp)[2]
  percent1=as.numeric(substr(a,11,12))
  percent2=as.numeric(substr(b,11,12))
  cluster=y
  colnames(acp)=c("Dimi", "Dimj")
  g= ggplot(acp, aes(Dimi,Dimj, color =sil, shape =cluster)) +
    geom_point(size=3) +   labs(x = paste("Dim", i,'---', percent1, "%"), y = paste("Dim", j,'---', percent2, "%"))+
    theme(text = element_text(family = "serif", size=14), title = element_text(color = "#8b0000"))

  return(ggplotly(g))

}


