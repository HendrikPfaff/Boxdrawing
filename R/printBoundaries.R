#' printBoundaries
#'
#' Ouputs the boundaries for all found boxes.
#'
#' @param box The result one box drawing algorithm (containing the boundaries).
#' @param colnames Names for each column/dimension of the data.
#' @param nonRelevant Flag if non relevant boundaries should be printed.
#' @return void if everything works fine.  NULL if no boundaries are found.
printBoundaries <- function(box, data=NULL, nonRelevant=FALSE){
  # Upper ideal should also be possible.
  nRuns <- length(box$lowerIdeal)
  
  if(nRuns > 0) {
    nBoxes <- nrow(box$lowerIdeal[[1]])
    nColumns <- nrow(t(box$lowerIdeal[[1]]))
    colnames <- colnames(data)
    
    for(run in 1:nRuns){
      writeLines(paste("Tradeoff:", box$tradeoff[run]))
      for(b in 1:nBoxes){
        writeLines(paste("\nBox", b))
        for(col in 1:nColumns){
          # Check for column names.
          if(!is.null(colnames[col])){
            name <- colnames[col]
          } else {
            name <- paste("Dimension", col)
          }
          
          # Check if the boundary should be printed.
          if(nonRelevant | (min(data[,col]) < box$lowerIdeal[[run]][b,col]) | (max(data[,col]) > box$upperIdeal[[run]][b,col])){
            # Check if both boundaries are the same.
            if(box$lowerIdeal[[run]][b,col] == box$upperIdeal[[run]][b,col]){
              writeLines(paste(name, "is", box$lowerIdeal[[run]][b,col]))
            } else {
              writeLines(paste(name, "is between", box$lowerIdeal[[run]][b,col], "and", box$upperIdeal[[run]][b,col]))
            }
          }
        }
      }
      writeLines("\n")
    }
  } else {
    return(NULL)
  }
}