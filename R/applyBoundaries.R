#' applyBoundaries
#' 
#' Generates a subset of the data by the given box boundaries.
#' @param box The result one box drawing algorithm (containing the boundaries).
#' @param data The underlying data.
#' @param complement Flag if the complement subset is needed.
#' @return Subset of the data if there is only one run or else list of subsets.
applyBoundaries <- function(box, data, complement=FALSE){
  # Just for the record: I'm pretty sure, there's some clever hack or one-liner for this stuff.
  
  # Upper ideal should also be possible.
  nRuns <- length(box$lowerIdeal)
  if(nRuns > 0) {
    nBoxes <- nrow(box$lowerIdeal[[1]])
    nColumns <- nrow(t(box$lowerIdeal[[1]]))
    clusters <- list()
    
    if(nRuns > 1){
      ret <- list()
    } else {
      ret <- NULL
    }
    
    for(run in 1:nRuns){
      for(b in 1:nBoxes){
        # There could be several boxes.
        clusters[[b]] <- data
        
        # Apply the boundaries for the current cluster/box.
        for(col in 1:nColumns){
          if(complement){
            clusters[[b]] <- subset(clusters[[b]], clusters[[b]][,col] < box$lowerIdeal[[run]][b,col] | clusters[[b]][,col] > box$upperIdeal[[run]][b,col])
          } else {
            clusters[[b]] <- subset(clusters[[b]], clusters[[b]][,col] >= box$lowerIdeal[[run]][b,col] & clusters[[b]][,col] <= box$upperIdeal[[run]][b,col])
          }
        }
      }
      
      # Combine the rows of all clusters.
      for(i in 1:length(clusters)){
        if(nRuns > 1){
          ret[[run]] <- rbind(ret[[run]], clusters[[i]])
        } else {
          ret <- rbind(ret, clusters[[i]])
        }
      }
    }
    return(ret)
  } else {
    return(NULL)
  }
}