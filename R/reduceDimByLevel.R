#'
#'
#'
reduceDimByLevel <- function(data, limit){
  # Show me the levels of all columns (To find the ones with the least).
  l <- list()
  for(i in 1:ncol(data)){
    l[i] <- length(levels(data[,i]))
  }
  colLevels <- data.frame(colnames=colnames(data), level=unlist(l))
  names <- as.character(colLevels[colLevels$level < limit,]$colnames)
  rm(i, l, colLevels)
  return(data[,names])
}