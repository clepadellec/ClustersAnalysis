#' Title
#'
#' @param df your dataframe including explanatory features and the group feature(target)
#' @param ind_group_class the indice of your group feature
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
#' @param ind_var_exp the qualitative explonatory feature
#'
#' @return the vtest value for each class(group feature)/modality(explonatory feature)
#' @export
#'
#' @examples
#'
desc_size_effect <- function(object,ind_var_exp){

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
#' @return all the chisq.test value between your qualitatives features and your group feature (with interpretation)
#' @export
#'
#' @examples
chisq_test_all <- function(object){
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
#' @param ind_var_exp
#'
#' @return
#' @export
#'
#' @examples
afc_plot <- function(object,ind_var_exp){
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
#' @param ind_var_exp
#'
#' @return
#' @export
#'
#' @examples
desc_profils <- function(object,ind_var_exp){
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
#' @param pop_a
#' @param pop_b
#'
#' @return
#' @export
#'
#' @examples
shapiro_test <- function(pop_a,pop_b){
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
#' @return
#' @export
#'
#' @examples
ttest_all <- function(object){
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
        hyp_gauss <- shapiro_test(pop_a[,j],pop_b[,j])
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
  colnames(res)<- colnames(data[object$ind.quan])
  return(res)
}


#' Title
#'
#' @param object your Univariate object
#' @param i
#'
#' @return
#' @export
#'
#' @examples
test.value=function(object,i=1){
  var_groupe <- object$name_group
  data=object$df[ ,object$ind.quan]

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
  print(moyenne_group)
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
#' @param x
#'
#' @return
#' @export
#'
#' @examples
type_variable=function(x){
  if (class(x)=='character'|length(unique(x))<7){
    type=('qualitative')
  } else{
    type=('quantitative')
  }
  return(type)
}

#' Title
#'
#' @param X
#'
#' @return
#' @export
#'
#' @examples
data_type=function(X){
  quali_quanti=sapply(X, FUN = type_variable)
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
#' @param data
#' @param rescale
#'
#' @return
#' @export
#'
#' @examples
dummy_data=function(data, rescale=FALSE){

  dataf=data

  col_names=colnames(data)
  t=sapply(data, type_variable)
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
matrix_distance=function(X,d){
  dist=pdist(X,d)
  return(dist)
}

#' Title
#'
#' @param X
#' @param y
#'
#' @return
#' @export
#'
#' @examples
mean_distance=function(X,y){

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
#' @param rescale
#' @param d
#'
#' @return
#' @export
#'
#' @examples
silhouette_ind=function(object,rescale=FALSE,d='euclidean'){
  indice= object$group

  X=object$df[,-indice]

  var_groupe <- object$name_group

  y=object$df[[var_groupe]]

  if (data_type(X)=="quantitatives"){
    X_bis=X
  }

  if (data_type(X)=="quantitative"){
    X_bis=data.frame(X)
  }

  if (data_type(X)=='quantitative-qualitative'|data_type(X)=='qualitatives'){
    X_bis=dummy_data(X,rescale)
  }

  if (data_type(X)=='qualitative'){
    X_bis=dummy_cols(data.frame(X), remove_first_dummy  = F)[,-1]
  }



  matrice_distance=matrix_distance(X_bis,d)
  moyenne_distance=mean_distance(matrice_distance,y)
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
#' @param rescale
#' @param d
#'
#' @return
#' @export
#'
#' @examples
silhouette_plot=function(object, rescale=FALSE, d="euclidean"){
  var_groupe <- object$name_group
  sil=silhouette_ind(object,rescale,d)
  y=object$df[[var_groupe]]
  df=data.frame("silhouette"=sil, "cluster"=y)

  # data frame contains the mean silhouette coefficient of clusters

  t=tapply(df[,"silhouette"], df[,"cluster"],mean)
  df_bis=data.frame("silhouette"=as.numeric(t), "cluster"=names(t))
  # plot
  g=ggplot(df, aes(cluster,silhouette,color=cluster))+geom_point(size=1.5)+ geom_point(data = df_bis, shape=15,size=4)+
    theme(text = element_text(family = "serif", size=14), title = element_text(color = "#8b0000"))+
    labs(title="Silhouette coefficient ") +ylim(-1,1)

  return(g)

}

#' Title
#'
#' @param x
#' @param g
#'
#' @return
#' @export
#'
#' @examples
eta2=function(x,g){
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
#' @return
#' @export
#'
#' @examples
fisher_test_all=function(object){
  var_groupe <- object$name_group
  data=object$df[ ,object$ind.quan]
  g=object$df[[var_groupe]]
  n=nrow(data)
  K=length(unique(g))
  Eta2=apply(data,MARGIN = 2,FUN = function(x){return(eta2(x,g))})
  Test_value=(n-K)/(K-1)*(Eta2)/(1-Eta2)
  p_value=1-pf(Test_value,K-1,n-K)
  df=data.frame('Eta2'=Eta2, 'Test_value'=Test_value, 'p_value'=p_value)
  return(df)
}

#' Title
#'
#' @param g1
#' @param g2
#'
#' @return
#' @export
#'
#' @examples
rand_index=function(g1,g2){
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
#' @return
#' @export
#'
#' @examples
kmean_rand_index=function(object){
  indice= object$group

  X=object$df[ ,object$ind.quan]
  var_groupe <- object$name_group

  y=object$df[[var_groupe]]
  n=length(unique(y))
  X_cr=scale(X,center = T,scale = T)
  n_means=kmeans(X_cr,centers = n,nstart = 5)

  rand=rand_index(n_means$cluster,y)
  return(rand)

}


