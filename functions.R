colorders <- function(inputtable, countinput){
  cols <- unique(tables_data[[paste0(countinput)]])
  for (column in cols) {
    inputtable[[column]] <- if (column %in% colnames(inputtable)) {
      inputtable[[column]]
    } else {
      NA
    }
    
  }
  return(inputtable)
}
