#' Title
#'
#' @param df your dataframe including explanatory features and the group feature(target)
#' @param ind_group_class the indice of your group feature
#'
#' @return an object which is going to be use to analyse clustering
#' @export
#'
#'
#' @examples
#' multivariate_object(iris,5)
multivariate_object=function(df,ind_group_class){
  # contrôle data.frame
  ok = is.data.frame(df)
  if (!ok){
    stop("Ce n'est pas un data frame")
  }

  # contrôle le nombre de colonnes
  nb_ok <- ncol(df)
  if (nb_ok < 2){
    stop("Il n'y a qu'une seule variable dans le data frame, vous pouvez utiliser la classe univarié")
  }

  # contrôle la taille de groupe

  size_g=length(df[,ind_group_class])
  if (size_g!=nrow(df)){
    stop('La taille de g est différente le nombre de ligne du data')
  }

  instance=list()
  ind.qual <- sapply(df, function(x) is.factor(x)| is.character(x))
  ind.quan <- sapply(df, function(x) is.numeric(x)| is.integer(x))
  instance$ind.qual <- ind.qual
  instance$ind.quan <- ind.quan
  df <- na.omit(df)
  df[,ind_group_class] <- as.character(df[,ind_group_class])
  instance$df <- df[,-ind_group_class]
  instance$group <- df[,ind_group_class]
  instance$lst_quali <- colnames(df[ ,ind.qual])
  instance$lst_quanti <- colnames(df[ ,ind.quan])

  class(instance)="multivariate_class"

  return(instance)

}


#' Verify the type of variables
#'
#' @param x a vector
#'
#' @return the type of x
#'
#' @examples
#' type_variable(iris$Sepal.Length)
type_variable=function(x){
  if (class(x)=='character'|length(unique(x))<7){
    type=('qualitative')
  } else{
    type=('quantitative')
  }
  return(type)
}


#' All type possible in the dataframe
#'
#' @param X dataframe or matrix
#'
#' @return the type of X: there are 5 possibilities: quantitative, quantitatives,
#' qualitative, qualitatives, quantitative-qualitative
#'
#' @examples
#' data_type(iris)
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


#' Title ...
#'
#' @param data a datafram
#' @param rescale a boolean
#'
#' @return data encoding with rescale or not
#'
#' @import fastDummies
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


#' Distance Matrix
#'
#' @param X a matrix or dataframe with explanatory variables
#' @param d the distance measure to be used and this must be "euclidean" or "L1"
#'
#' @return the distance matrix computed by using the euclidean distance or L1 distance between the rows of X.
#'
#' @import rdist
#'
#' @examples
#' matrix_distance(iris[,c("Sepal.Length","Sepal.Width")],d="euclidean")
matrix_distance=function(X,d){
  dist=pdist(X,d)
  return(dist)
}


#' Title ...
#'
#' @param X a numeric symmetric matrix containing the pairwise distance between the rows of a data frame
#' @param y a factor such that length(X)=length(y)
#'
#' @return a data frame containing the average distance of each row to all observations of each cluster C
#'
#'
#' @examples
#' ...
mean_distance=function(X,y){

  m=nrow(X)
  distance=c()
  for (i in 1:m){
    distance=rbind(distance,tapply(X[i,-i],y[-i], mean))
  }

  return(distance)

}


#' Title ...
#'
#' @param object your Multavariate object
#' @param rescale
#' @param d method used
#'
#' @return Silhouette Coefficient of each row
#' @export
#'
#' @examples
#' ...
silhouette_ind_multi=function(object,rescale=FALSE,d='euclidean'){
  X=object$df
  y=object$group

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


#' Values ACP with 2 dimensions
#' @param X ...
#' @param i ...
#' @param j ...
#' @param rescale ...
#'
#' @return the values of ACP with 2 dimensions
#' @import  FactoMineR
#'
#' @examples
#' acp_2_axes(iris[,-5])
acp_2_axes=function(X,i=1,j=2, rescale=FALSE){

  if (i==0|j==0|i>ncol(X) | j>ncol(X) ){
    return("the index must be larger than 0 and smaller than the number of variables")
    stop()
  }


  #  if (data_type(X)=="quantitatives"){
  #    X_bis=X
  #  }

  #  if (data_type(X)=="quantitative"){
  #    X_bis=data.frame(X)
  #  }

  #  if (data_type(X)=='quantitative-qualitative'|data_type(X)=='qualitatives'){
  #    X_bis=dummy_data(X,rescale)
  #  }

  #  if (data_type(X)=='qualitative'){
  #    X_bis=dummy_cols(X, remove_first_dummy  = F)[,-1]
  #  }


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


#' Title ...
#' @param object your Multivariate object
#' @param i ...
#' @param j ...
#' @param rescale ...
#' @param d method used
#'
#' @return
#'
#' @import  ggplot2
#' @import FactoMineR
#' @export
#'
#' @examples
#' ...
sil_pca_plot_multi=function(object,i=1,j=2, rescale=FALSE, d="euclidean"){

  X=object$df
  y=object$group

#  if (class(y)!="factor"){
#    return("y must be a factor")
#    stop()
#  }

  if (i==0|j==0|i>ncol(X) | j>ncol(X) ){
    return("the index must be larger than 0 and smaller than the number of variables")
    stop()
  }


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
    X_bis=dummy_cols(X, remove_first_dummy  = F)[,-1]
  }



  acp=acp_2_axes(X_bis,i,j)
  sil=silhouette_ind_multi(object,rescale,d)
  a=colnames(acp)[1]
  b=colnames(acp)[2]
  percent1=as.numeric(substr(a,11,12))
  percent2=as.numeric(substr(b,11,12))
  cluster=y
  colnames(acp)=c("Dimi", "Dimj")
  g= ggplot(acp, aes(Dimi,Dimj, color =sil, shape =cluster)) +
    geom_point(size=3) +   labs(x = paste("Dim", i,'---', percent1, "%"), y = paste("Dim", j,'---', percent2, "%"))+
    theme(text = element_text(family = "serif", size=14), title = element_text(color = "#8b0000"))

  return(g)

}


#' Title ...
#' @param object your Multivariate object
#' @param rescale ...
#' @param d method used
#'
#' @return the silhouette plot
#'
#' @export
#' @import ggplot2
#'
#' @examples
#' ...
silhouette_plot_multi=function(object, rescale=FALSE, d="euclidean"){

  X=object$df
  y=object$group

  sil=silhouette_ind_multi(object,rescale,d)
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


#' Title ...
#'
#' @param object your Multivariate object
#' @param i i-th cluster
#' @return a dataframe with a summary of value test between the explanatory variables and and the i-th cluster
#' @export
#'
#' @examples
#' obj = multivariate_object(infert,1)
#' test.value(obj)
test.value=function(object, i=1){

  data=object$df
  g=object$group

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


#' Title ...
#'
#' @param g1 first cluster
#' @param g2 second cluster
#'
#' @return Rand index measure to compare the similarity of two clustering.
#'
#' @examples
#' ...
rand_index_multi=function(g1,g2){
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


#' Title ...
#'
#' @param object your Multivariate object
#'
#' @return rand index between the result of kmean and y
#' @export
#'
#' @examples
#' obj = multivariate_object(infert,1)
#' kmean_rand_index_multi(obj)
kmean_rand_index_multi=function(object){
  X=object$df
  y=object$group
  n=length(unique(y))
  X_cr=scale(X,center = T,scale = T)
  n_means=kmeans(X_cr,centers = n,nstart = 5)

  rand=rand_index_multi(n_means$cluster,y)
  return(rand)

}


#' Title ...
#' @param object your Multivariate object
#' @param i ...
#' @param j ...
#'
#' @return a plot about the cluster on the first two dimensions
#' @import gpplot2
#' @export
#'
#' @examples
#' obj = multivariate_object(infert,1)
#' kmean_clustering_plot_multi(obj)
kmean_clustering_plot_multi=function(object,i=1,j=2){

  X=object$df
  y=object$group

#  if (class(y)!="factor"){
#    return("y must be a factor")
#    stop()
#  }

  if (i==0|j==0|i>ncol(X) | j>ncol(X) ){
    return("the index must be larger than 0 and smaller than the number of variables")
    stop()
  }

  acp=acp_2_axes(X,i,j)
  a=colnames(acp)[1]
  b=colnames(acp)[2]
  percent1=as.numeric(substr(a,11,12))
  percent2=as.numeric(substr(b,11,12))
  cluster=y
  colnames(acp)=c("Dimi", "Dimj")


  n=length(unique(y))
  X_cr=scale(X,center = T,scale = T)
  n_means=kmeans(X_cr,centers = n,nstart = 5)
  cluster_kmean=n_means$cluster

  g= ggplot(acp, aes(Dimi,Dimj, color =cluster_kmean, shape =cluster)) +
    geom_point(size=3) +   labs(x = paste("Dim", i,'---', percent1, "%"), y = paste("Dim", j,'---', percent2, "%"))+
    theme(text = element_text(family = "serif", size=14), title = element_text(color = "#8b0000"))

  return(g)

}


#' Rapport Correlation Multivariate
#'
#' @param object your Multivariate object
#' @param method method used
#'
#' @return rapport correlation
#'
#' @import FactoMineR
#' @export
#'
#' @examples
#' obj = multivariate_object(infert,1)
#' R2_multivariate_multi(object = obj)
R2_multivariate_multi=function(object, method='encoding'){

  data=object$df
  g=object$group

  if (data_type(data)=='qualitatives'){
    if (method=='encoding'){
      data_bis=dummy_data(data)
    } else{
      p=ncol(data)
      M=sum(sapply(data, FUN = function(x){return(length(unique(x)))}))
      n_acm=M-p
      ACM=MCA(data, ncp = n_acm, graph = FALSE)
      data_bis=ACM$ind$coord
    }
  } else{
    data_bis=data
  }




  G=apply(data_bis, MARGIN = 2, FUN = mean)
  n_g=tapply(data_bis[,1],g,FUN = length)
  n_G=length(unique(g))
  columns=ncol(data_bis)
  row=nrow(data_bis)
  barycentre=c()
  for (i in 1:columns){
    barycentre=rbind(barycentre,tapply(data_bis[,i], g, FUN = mean))
  }

  S=0
  for (i in 1:n_G){
    s=as.numeric(n_g[i])*sum((as.numeric(barycentre[,i])-as.numeric(G))^2)
    S=S+s
  }

  matrix_G=c()
  for (j in 1:row){
    matrix_G=rbind(matrix_G,G)
  }

  Inertie_total=sum((matrix_G-data_bis)^2)

  valeur_test=S/Inertie_total



  return(valeur_test)
}


#' Plot ACM multivariate
#'
#' @param object your Multivariate object
#' @param dims dimensions to print
#' @param name_ind rownames to apply
#' @param qtsup if users want to add a quantitative features to acm (could be a number or vector of numbers)
#' @import FactoMineR
#' @import factoextra
#' @import corrplot
#' @return corrplot with the contributions of features for each dimensions and a biplot to vizualize acm
#'
#' @examples
#' CO2_bis = CO2[,1:3]
#' obj = multivariate_object(CO2_bis,1)
#' acm_plot_multi(obj)
acm_plot_multi <- function(object,dims=c(1,2),name_ind=0, qtsup=NULL){

  df=object$df

  if (data_type(df)!= 'qualitatives'){
    return('dataframe must contain qualitatives features')
    stop()
  }

  #si name_ind est différent de zero, alors on affecte les rownames au df
  if (name_ind != 0){
    rownames(df)=name_ind
  }

  #creation de l'acm sur le dataframe,
  res.mca <- MCA (df, graph = FALSE, quanti.sup=qtsup)

  #on recupère les résultats pour les individus
  ind <- get_mca_ind (res.mca)
  #et pour les variables
  var <- get_mca_var(res.mca)
  #on affiche un corrplot pour visualiser les cos carré
  corrplot(var$cos2, is.corr=FALSE)

  #en fonction de s'il y a des rownames ou non on affiche un graph différent
  if (name_ind != 0){
    fviz_mca_biplot(res.mca, select.ind = list(cos2 = 75),axes=dims,
                    repel = TRUE,geom=c("text", "text"))
  }else{
    fviz_mca_biplot(res.mca, select.ind = list(cos2 = 75),axes=dims,
                    repel = TRUE,geom=c("point", "text"))
  }



}

