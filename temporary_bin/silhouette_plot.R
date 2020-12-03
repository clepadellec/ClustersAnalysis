#' Title
#' @param X a matrix or dataframe with explanatory variables
#' @param y predicted labels for each row of X. It should be a factor
#' @param d the distance measure to be used and this must be "euclidean" or "L1"
#'
#' @return the silhouette plot
#'
#' @export
#' @import ggplot2
#'
#' @examples
silhouette_plot=function(X,y, rescale=FALSE, d="euclidean"){




  sil=silhouette_ind(X,y,rescale,d)
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


