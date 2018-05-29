#' Reducing the negative Observations that are too different from the positive.
#' 
reduceObsByHemming <- function(pos, neg, maxDist){
  keep <- c()
  for(nRow in 1:nrow(neg)){
    for(pRow in 1:nrow(pos)){
      dist <- sum(neg[nRow,] != pos[pRow,])
      if(dist <= maxDist & !(nRow %in% keep)){
        keep <- c(keep,nRow)
      }
    }
  }
  return(neg[keep,])
}