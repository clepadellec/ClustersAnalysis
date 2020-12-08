##############################################################################
#                                 CONSTRUCTOR                                #-----------------------
##############################################################################



#' Constructor for multivariate object
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

  #df_without_grp <- df[,-ind_group_class]
  instance=list()

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
  instance$df <- df[,-ind_group_class]
  instance$group <- df[,ind_group_class]
  instance$lst_quali <- colnames(df[ ,ind.qual])
  instance$lst_quanti <- colnames(df[ ,ind.quan])

  class(instance)="multivariate_class"

  return(instance)

}


##############################################################################
#                                 Internal Measures                          #-----------------------
##############################################################################

###
#1#---
###

#' Verify the type of variables
#'
#' @param x a vector
#'
#' @return the type of x
#' @export
#'
#' @examples
#' m_type_variable(iris$Sepal.Length)
m_type_variable=function(x){
  #s'il sagit d'un facteur ou character on est bien sur une variable quali
  if (is.factor(x)==TRUE|is.character(x)==TRUE){
    type='qualitative'
  }else{
    type='quantitative'
    }

  return(type)
}

###
#2#---
###

#' All type possible in the dataframe
#'
#' @param X dataframe or matrix
#'
#' @return the type of X: there are 5 possibilities: quantitative, quantitatives,
#' qualitative, qualitatives, quantitative-qualitative
#'
#' @export
#' @examples
#' m_data_type(iris)
m_data_type=function(X){

  #déterminer le type des variables dans le dataframe
  quali_quanti=sapply(X, FUN = m_type_variable)
  tab=table(quali_quanti)

  #déterminer le type du data en fonction du nombre des variables qualitatives/quantitatives
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

###
#3#---
###

#' one hot recoding
#'
#' @param data a datafram
#' @param rescale a boolean
#'
#' @return data encoding with rescale or not
#'
#' @import fastDummies
#' @export
#' @examples
m_dummy_data=function(data, rescale=FALSE){

  dataf=data


  #nom des variables du data
  col_names=colnames(data)

  #déterminer le type des variables
  t=sapply(data, m_type_variable)

  #nom des variables qualitatives
  variable_qualitative=colnames(data)[t=="qualitative"]

  #nom des variables quantitatives
  variable_quantitative=colnames(data)[t=="quantitative"]

  #encoding one-hot si il y a au moins une variable qualitative, sinon on ne fait rien
  if (length(variable_qualitative)==0){
#    return("There is no qualitative variable in this data")
#    break
    datafbis=dataf
  } else{
    datafbis= dummy_cols(dataf, select_columns = variable_qualitative, remove_selected_columns = TRUE)
  }

datafbisbis=datafbis


  #rescale des variables quantitatives si il y a au moins une variable quantitative et rescale=True, sinon on ne fait rien
  if (rescale==TRUE){
    if (length(variable_quantitative)==0){
#      return("There is no quantitative variable in this data")
#      break
      datafbisis=datafbis
    } else{
      for (i in variable_quantitative){
        datafbisbis[i]=scale(datafbis[i], center = T, scale = T)
      }
    }
  }
  return(datafbisbis)
}

###
#4#---
###

#' Distance Matrix
#'
#' @param X a matrix or dataframe with explanatory variables
#' @param d the distance measure to be used and this must be "euclidean" or "L1"
#'
#' @return the distance matrix computed by using the euclidean distance or L1 distance between the rows of X.
#'
#' @import rdist
#' @export
#'
#' @examples
#' m_matrix_distance(iris[,c("Sepal.Length","Sepal.Width")],d="euclidean")
m_matrix_distance=function(X,d){

  #une matrice de distance calculé entre deux individus en utilisant la fonction pdist du package rdist
  dist=pdist(X,d)
  return(dist)
}

###
#5#---
###

#' Mean distance
#'
#' @param X a numeric symmetric matrix containing the pairwise distance between the rows of a data frame
#' @param y a factor such that length(X)=length(y)
#'
#' @return a data frame containing the average distance of each row to all observations of each cluster C
#' @export
#'
#' @examples  m_mean_distance(m_matrix_distance(iris[,c("Sepal.Length","Sepal.Width")],d="euclidean"), iris[,'Species'])
#'
m_mean_distance=function(X,y){

  #nombre des individus
  m=nrow(X)

  #liste vide des distances
  distance=c()

  #calculer la distance moyenne entre chaque individu à chaque group
  for (i in 1:m){
    distance=rbind(distance,tapply(X[i,-i],y[-i], mean))
  }

  return(distance)

}

###
#6#---
###

#' Calculate index silhouette
#'
#' @param object your Multavariate object
#' @param rescale
#' @param d method used
#'
#' @return Silhouette Coefficient of each row
#' @export
#'
#' @examples m_silhouette_ind(multivariate_object(iris,5))
#'
m_silhouette_ind=function(object,rescale=FALSE,d='euclidean'){

  X=object$df
  y=object$group

  #si le data ne contient que des variables quantitatives, on ne fait rien
  if (m_data_type(X)=="quantitatives"){
    X_bis=X
  }

  #si le data ne contient qu'une seule variable quantitative, on le transforme en dataframe
  if (m_data_type(X)=="quantitative"){
    X_bis=data.frame(X)
  }

  #si le data contient à la fois des variables quantitatives et qualitatives, on le transforme en utilisant la fonction dummy_data
  if (m_data_type(X)=='quantitative-qualitative'|m_data_type(X)=='qualitatives'){
    X_bis=m_dummy_data(X,rescale)
  }

  #si le data contient qu'une seule variable qualitative, on utilise encoding one-hot
  if (m_data_type(X)=='qualitative'){
    X_bis=dummy_cols(data.frame(X), remove_first_dummy  = F)[,-1]
  }


  # récuperer matrice distance
  matrice_distance=m_matrix_distance(X_bis,d)

  # récuperer la moyenne distance
  moyenne_distance=m_mean_distance(matrice_distance,y)
  sil=c()

  #nombre des individus
  m=nrow(moyenne_distance)

  #calculer le coefficient de silhouette de chaque individu
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

###
#7#---
###


#' Values ACP with 2 dimensions
#' @param X
#' @param i
#' @param j
#' @param rescale
#'
#' @return the values of ACP with 2 dimensions
#' @import  FactoMineR
#' @export
#'
#' @examples
#' m_acp_2_axes(iris[,-5])
m_acp_2_axes=function(X,i=1,j=2, rescale=FALSE){


  #si un des deux indices sont plus grand que le nombre de variable ou egale à 0, on arrete
  if (i==0|j==0|i>ncol(X) | j>ncol(X) ){
    return("the index must be larger than 0 and smaller than the number of variables")
    stop()
  }


  #effectuer un ACP sur X
  acp=PCA(X,graph=FALSE)

  #coordonne des individus sur les composantes factoriels
  acp.ind=acp$ind
  acp.ind.cord=acp.ind$coord

  #creer un dataframe qui contient des coordonnes sur le i-ieme et j-ieme axe
  df=acp.ind.cord[,c(i,j)]
  dfbis=data.frame(df)

  #pourcentage d'inertie du i-ieme composant
  PC1=round(acp$eig[,2][i])

  #pourcentage d'inertie du j-ieme composant
  PC2=round(acp$eig[,2][j])

  #renommer le nombre des colonnes du dfbis
  colnames(dfbis)=c(paste("Dim",i,"---",PC1,"%"), paste("Dim",j,"---",PC2,"%"))
  return(dfbis)
}

###
#8#---
###

#' Plot silhouette index with Component analysis
#'
#' @param object your Multivariate object
#' @param i
#' @param j
#' @param rescale
#' @param d method used
#'
#' @return
#' @rawNamespace import(plotly, except = last_plot)
#' @import  ggplot2
#' @import FactoMineR
#' @export
#'
#' @examples m_sil_pca_plot(multivariate_object(iris,5))
#'
m_sil_pca_plot=function(object,i=1,j=2, rescale=FALSE, d="euclidean", interact=TRUE){

  X=object$df
  y=object$group


  #si un des deux indices sont plus grand que le nombre de variable ou egale à 0, on arrete
  if (i==0|j==0|i>ncol(X) | j>ncol(X) ){
    return("the index must be larger than 0 and smaller than the number of variables")
    stop()
  }

  #si le data ne contient que des variables quantitatives, on ne fait rien
  if (m_data_type(X)=="quantitatives"){
    X_bis=X
  }

  #si le data ne contient qu'une seule variable quantitative, on le transforme en dataframe
  if (m_data_type(X)=="quantitative"){
    X_bis=data.frame(X)
  }

  #si le data contient à la fois des variables quantitatives et qualitatives, on le transforme en utilisant la fonction dummy_data
  if (m_data_type(X)=='quantitative-qualitative'|m_data_type(X)=='qualitatives'){
    X_bis=m_dummy_data(X,rescale)
  }

  #si le data contient qu'une seule variable qualitative, on utilise encoding one-hot
  if (m_data_type(X)=='qualitative'){
    X_bis=dummy_cols(X, remove_first_dummy  = F)[,-1]
  }


  #Récuperer les coordonnés sur le i-ieme et j-ieme composante en utilisant la fonction m_acp_2_axes
  acp=m_acp_2_axes(X_bis,i,j)

  #récuperer les silhouettes en utilisant la fonction m_silhouette_ind
  sil=m_silhouette_ind(object,rescale,d)


  a=colnames(acp)[1]
  b=colnames(acp)[2]

  percent1=as.numeric(substr(a,11,12))
  percent2=as.numeric(substr(b,11,12))
  cluster=y
  colnames(acp)=c("Dimi", "Dimj")

  #effectuer une representation graphique sur i-ieme et j-ieme composante
  g= ggplot(acp, aes(Dimi,Dimj, color =sil, shape =cluster)) +
    geom_point(size=3) +   labs(x = paste("Dim", i,'---', percent1, "%"), y = paste("Dim", j,'---', percent2, "%"))+
    theme(text = element_text(family = "serif", size=14), title = element_text(color = "#8b0000"))+
    labs(title="ACP and Silhouette")

  if (interact==TRUE){return(ggplotly(g))}else{return(g)}

}

###
#8#---
###


#' Plot silhouette index
#'
#' @param object your Multivariate object
#' @param rescale
#' @param d method used
#'
#' @return the silhouette plot
#' @rawNamespace import(plotly, except = last_plot)
#' @export
#' @import ggplot2
#'
#' @examples m_silhouette_plot(multivariate_object(iris,5))
#'
m_silhouette_plot=function(object, rescale=FALSE, d="euclidean", interact=TRUE){

  #mise en forme des données
  X=object$df
  y=object$group

  #calcul des indices de silhouettes
  sil=m_silhouette_ind(object,rescale,d)

  #création d'un df avec les classes et leurs indices associés
  df=data.frame("silhouette"=sil, "cluster"=y)


  # data frame contenant la moyenne des indices de silhouettes par classes
  t=tapply(df[,"silhouette"], df[,"cluster"],mean)
  df_bis=data.frame("silhouette"=as.numeric(t), "cluster"=names(t))
  # plot
  g=ggplot(df, aes(cluster,silhouette,color=cluster))+geom_point(size=1.5)+ geom_point(data = df_bis, shape=15,size=4)+
    theme(text = element_text(family = "serif", size=14), title = element_text(color = "#8b0000"))+
    labs(title="Silhouette coefficient ") +ylim(-1,1)

  if (interact==TRUE){return(ggplotly(g))}else{return(g)}

}

###
#9#---
###

#' Davies-Boulin Indice
#'
#' @param object your Multivariate object
#' @param
#'
#' @return Indice de Davies-Bouldin
#'
#' @import FactoMineR
#' @export
#'
#' @examples m_DB_index(multivariate_object(infert,1))
m_DB_index=function(object, method='encoding', rescale=FALSE){

  # prétraitement de données

  data=object$df
  g=object$group

  data_bis=data

  #si il existe une mixité au niveau des types de variables, on recode les qualis en one-hot
  if (m_data_type(data)=='quantitative-qualitative'){
    data_bis=m_dummy_data(data,rescale)
  }

  #si il n'y a que des quali, on recode puis on effectue une acm et on récupère les coordonnées des individus dans le plan
  if (m_data_type(data)=='qualitatives'){
    if (method=='encoding'){
      data_bis=m_dummy_data(data,rescale)
    } else{
      p=ncol(data)
      M=sum(sapply(data, FUN = function(x){return(length(unique(x)))}))
      n_acm=M-p
      ACM=MCA(data, ncp = n_acm, graph = FALSE)
      data_bis=ACM$ind$coord
    }

  }





  G=apply(data_bis, MARGIN = 2, FUN = mean)
  n_g=tapply(data_bis[,1],g,FUN = length)
  n_G=length(unique(g))
  columns=ncol(data_bis)
  row=nrow(data_bis)
  barycentre=c()
  #calcul des barycentres
  for (i in 1:columns){
    barycentre=rbind(barycentre,tapply(data_bis[,i], g, FUN = mean))
  }


  nom_cluster=colnames(barycentre)
  # calcul distance intra-classe
  intra_DB=c()

  for (nom in nom_cluster){
    df_dataframe=data_bis[g==nom,]
    len_df_dataframe=nrow(df_dataframe)
    S=1/len_df_dataframe* sum(apply(df_dataframe,MARGIN = 1,FUN = function(x){return(sum((x-as.numeric(barycentre[,nom]))^2))}))

    intra_DB=c(intra_DB,S)
  }
  intra_distance_DB=data.frame(intra_DB)
  colnames(intra_distance_DB)="Indice de Davies Bouldin"
  rownames(intra_distance_DB)=nom_cluster

  # calcul distance entre des barycentres
  dist=pdist(t(barycentre))


  # calcul de l'indice DB
  DB=c()

  for (i in 1:n_G){
    s=(intra_distance_DB[-i,]+intra_distance_DB[i,])/dist[i,-i]
    DB=c(DB,max(s))
  }

  indice_DB_final=data.frame(DB)
  colnames(indice_DB_final)='Indice de Davies Bouldin'
  rownames(indice_DB_final)=nom_cluster
  indice_DB_final


  return(indice_DB_final)
}

##############################################################################
#                                 Test Value                                 #-----------------------
##############################################################################


#' Calculate the test value
#'
#' @param object your Multivariate object
#' @param i i-th cluster
#' @return a dataframe with a summary of value test between the explanatory variables and and the i-th cluster
#' @export
#'
#' @examples m_test.value(multivariate_object(infert,1))
m_test.value=function(object, i=1){

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
  #calcul de la variance
  variance=apply(data,MARGIN = 2,FUN = function(x){return((n-1)/n*var(x))})
  #calcul des moyennes par groupes
  moyenne_group=t(apply(data,MARGIN = 2, FUN=function(x){return(tapply(x,g,FUN=mean))}))
  moyenne=apply(data, MARGIN = 2,mean)
  len=tapply(data[,1],g, FUN = length)
  for ( j in 1:n_unique){
    #calcul de la valeur test
    VT=(moyenne_group[,j]-moyenne)/sqrt(((n-len[j])/(n-1)*(variance/len[j])))
    pvalue=rep(0,length(VT))
    #puis de la p-value associée
    for (k in 1:length(VT)){
      if (VT[k]<0){
        pvalue[k]=2*pnorm(VT[k])
      } else{
        pvalue[k]=2*(1-pnorm(VT[k]))
      }
    }
    #mise en place d'un df ou on retournera la valeur test et la p-value
    df=data.frame(VT, pvalue)
    row.names(df)=colnames(data)
    colnames(df)[1]=as.character(unique(g)[j])
    df_sort=df[order(df$pvalue, decreasing = FALSE),]
    l[[j]]=df_sort
  }

  return(l[[i]])

}

##############################################################################
#                                 External Measures                          #-----------------------
##############################################################################

###
#1#---
###

#' Calculate rand index
#'
#' @param g1 first cluster
#' @param g2 second cluster
#'
#' @return Rand index measure to compare the similarity of two clustering.
#'
#' @examples
#'
m_rand_index=function(g1,g2){
  if (length(g1)!= length(g2)){
    return("g1 and g2 must have the same length")
    stop()
  }

  # a est le nombre des paires (x1,x2) groupés dans Y1 et également groupés dans Y2
  a=0
  # b est le nombre des paires (x1,x2) qui sont séparés dans Y1 et dans Y2
  b=0
  c=0
  d=0
  n=length(g1)
  #calcul de a et b
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

  #calcul de l'indice de rand
  rand=(a+b)/((n*(n-1)/2))

  return(rand)
}



###
#2#---
###



#' Calculate adjusted rand index
#'
#' @param g1 first cluster
#' @param g2 second cluster
#'
#' @return ajusted Rand index measure to compare the similarity of two clustering.
#' @export
#'
#' @examples
#'
m_rand_ajusted=function(g1,g2){
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
      if ((g1[i]==g1[j]) & (g2[i]!=g2[j])){
        c=c+1
      }
      if ((g1[i]!=g1[j]) & (g2[i]==g2[j])){
        c=c+1
      }
    }
  }
  #calculs des différents termes puis de l'indice de rand ajusté
  rand1=a-(a+c)*(a+d)/(a+b+c+d)
  rand2=1/2*(2*a+c+d)-(a+c)*(a+d)/(a+b+c+d)
  rand=rand1/rand2

  return(rand)
}

###
#3#---
###

#' Compare two partitions, real classes vs kmeans classes (with ajusted indice)
#'
#' @param object your Multivariate object
#'
#' @return ajusted rand index between the result of kmean and y
#' @export
#'
#' @examples m_kmean_rand_ajusted(multivariate_object(infert,1))
#'
m_kmean_rand_ajusted=function(object, rescale=FALSE){
  X=object$df
  y=object$group


  #on effectue un recodage one-hot si il y a des variables quali
  if (m_data_type(X)=="quantitatives"){
    X_bis=X
  }

  if (m_data_type(X)=="quantitative"){
    X_bis=data.frame(X)
  }

  if (m_data_type(X)=='quantitative-qualitative'|m_data_type(X)=='qualitatives'){
    X_bis=m_dummy_data(X,rescale)
  }

  if (m_data_type(X)=='qualitative'){
    X_bis=dummy_cols(X, remove_first_dummy  = F)[,-1]
  }



  n=length(unique(y))
  #centrage et réduction des valeurs
  X_cr=scale(X_bis,center = T,scale = T)
  #mise en place du kmeans
  n_means=kmeans(X_cr,centers = n,nstart = 5)

  #calcul de l'indice de rand ajusté
  rand=m_rand_ajusted(n_means$cluster,y)
  return(rand)

}

###
#4#---
###

#' Compare two partitions, real classes vs kmeans classes
#'
#' @param object your Multivariate object
#'
#' @return rand index between the result of kmean and y
#' @export
#'
#' @examples m_kmean_rand_index(multivariate_object(infert,1))
#'
m_kmean_rand_index=function(object, rescale=FALSE){
  X=object$df
  y=object$group

  #exactement la même démarche que l'algorithme du dessus mis à part le fait qu'on calcule l'indice de rand et non l'indice de rand ajusté
  if (m_data_type(X)=="quantitatives"){
    X_bis=X
  }

  if (m_data_type(X)=="quantitative"){
    X_bis=data.frame(X)
  }

  if (m_data_type(X)=='quantitative-qualitative'|m_data_type(X)=='qualitatives'){
    X_bis=m_dummy_data(X,rescale)
  }

  if (m_data_type(X)=='qualitative'){
    X_bis=dummy_cols(X, remove_first_dummy  = F)[,-1]
  }



  n=length(unique(y))
  X_cr=scale(X_bis,center = T,scale = T)
  n_means=kmeans(X_cr,centers = n,nstart = 5)

  rand=m_rand_index(n_means$cluster,y)
  return(rand)

}



###
#5#---
###


#' plot cluster on the first two dimensions
#' @param object your Multivariate object
#' @param i
#' @param j
#'
#' @return a plot about the cluster on the first two dimensions
#' @rawNamespace import(plotly, except = last_plot)
#' @import ggplot2
#' @export
#'
#' @examples m_kmean_clustering_plot(multivariate_object(infert,1))
#'
m_kmean_clustering_plot=function(object,i=1,j=2, rescale=FALSE, interact=TRUE){

  X=object$df
  y=object$group

  #recodage one-hot au besoin
  if (m_data_type(X)=="quantitatives"){
    X_bis=X
  }

  if (m_data_type(X)=="quantitative"){
    X_bis=data.frame(X)
  }

  if (m_data_type(X)=='quantitative-qualitative'|m_data_type(X)=='qualitatives'){
    X_bis=m_dummy_data(X,rescale)
  }

  if (m_data_type(X)=='qualitative'){
    X_bis=dummy_cols(X, remove_first_dummy  = F)[,-1]
  }




  #condition d'arret
  if (i==0|j==0|i>ncol(X) | j>ncol(X) ){
    return("the index must be larger than 0 and smaller than the number of variables")
    stop()
  }


  #on effectue l'acp sur les deux axes renseignés
  acp=m_acp_2_axes(X_bis,i,j)
  a=colnames(acp)[1]
  b=colnames(acp)[2]
  #On récupère les pourcentage d'explication par dimensions
  percent1=as.numeric(substr(a,11,12))
  percent2=as.numeric(substr(b,11,12))
  cluster=y
  colnames(acp)=c("Dimi", "Dimj")


  n=length(unique(y))
  #centrage et réduction des données
  X_cr=scale(X_bis,center = T,scale = T)
  #mise en place du kmeans
  n_means=kmeans(X_cr,centers = n,nstart = 5)

  cluster_kmean=n_means$cluster

  #plot
  g= ggplot(acp, aes(Dimi,Dimj, color =cluster_kmean, shape =cluster)) +
    geom_point(size=3) +   labs(x = paste("Dim", i,'---', percent1, "%"), y = paste("Dim", j,'---', percent2, "%"))+
    theme(text = element_text(family = "serif", size=14), title = element_text(color = "#8b0000"))

  if (interact==TRUE){return(ggplotly(g))}else{return(g)}

}

##############################################################################
#                                 R square                                   #-----------------------
##############################################################################



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
#' @examples m_R2_multivariate(multivariate_object(infert,1))
m_R2_multivariate=function(object, method='encoding', rescale=FALSE){

  #mise en place des données
  data=object$df
  g=object$group

  data_bis=data

  #recodage des données quali au besoin
  if (m_data_type(data)=='quantitative-qualitative'){
      data_bis=m_dummy_data(data,rescale)
  }

  if (m_data_type(data)=='qualitatives'){
    if (method=='encoding'){
      data_bis=m_dummy_data(data,rescale)
    } else{
      #si plusieurs quali alors on effectue une ACM et on recupère les coordonnées des individus dans le plan
      p=ncol(data)
      M=sum(sapply(data, FUN = function(x){return(length(unique(x)))}))
      n_acm=M-p
      ACM=MCA(data, ncp = n_acm, graph = FALSE)
      data_bis=ACM$ind$coord
    }

  }



  #calcul des moyennes
  G=apply(data_bis, MARGIN = 2, FUN = mean)
  n_g=tapply(data_bis[,1],g,FUN = length)
  n_G=length(unique(g))
  columns=ncol(data_bis)
  row=nrow(data_bis)
  barycentre=c()
  #calcul des barycentres
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

  #calcul de la valeur test
  valeur_test=S/Inertie_total



  return(valeur_test)
}

##############################################################################
#                        Multiple component analysis                         #-----------------------
##############################################################################



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
#' @export
#'
#' @examples m_acm_plot(multivariate_object(CO2[,1:3],1))
m_acm_plot <- function(object,dims=c(1,2),name_ind=0, qtsup=NULL){

  df=object$df

  if (m_data_type(df)!= 'qualitatives'){
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










































