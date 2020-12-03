
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
#' @param df
#' @param var_groupe
#'
#' @return
#' @export
#'
#' @examples
ttest_all <- function(df,var_groupe){
  df <- na.omit(df)
    type_var <- df %>% head %>% collect %>% lapply(class) %>% unlist
    # Repère index var quanti
    var_quanti <- c(which(type_var == "numeric" | type_var == "integer"))
    # Supprime var quanti
    data <- df[,c(var_quanti)]
    var_groupe <-df[[var_groupe]]
    data <- cbind(data,var_groupe)
    lst_grp<-unique(var_groupe)
    #print(data$var_groupe)
    res <- matrix(data = NA, nrow = length(lst_grp), ncol = length(var_quanti))
    lig=1
    for (i in lst_grp){
      col=1
      new_data=data
      new_data$var_groupe[new_data$var_groupe != i] <- "others"
      pop_a=new_data[new_data$var_groupe==i,]
      pop_b=new_data[new_data$var_groupe!=i,]
      gauss=TRUE
      if(nrow(pop_a)<30|nrow(pop_b)<30){gauss=FALSE}

      for (j in 1:length(var_quanti)){
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
    colnames(res)<- colnames(data[var_quanti])
    return(res)
}


