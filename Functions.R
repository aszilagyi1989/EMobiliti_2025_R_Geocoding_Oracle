columns_list <- function(dataframe, numberofColumns){
  
  values <<- ""
  columns <<- ""
  for(i in 1:numberofColumns){
    
    if (i != numberofColumns){
      
      values <<- paste0(values, ":", i, ", ")
      columns <<- paste0(columns, colnames(dataframe)[i], ", ")
      
    }else{
      
      values <<- paste0(values, ":", i)
      columns <<- paste0(columns, colnames(dataframe)[i])
      
    }
    
  }
  
}
