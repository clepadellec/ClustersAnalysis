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


