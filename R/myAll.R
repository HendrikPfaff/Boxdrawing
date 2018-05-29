#' myAll
#' 
#' R equivalent of the matlab function all(A, 2). Checks of a row of Data is nonzero or true.
#' 
#' @param x Matrix of Data.
#' @return Vector containing TRUE or FALSE.
myAll <- function(x){
  ret <- matrix(0, nrow(x), 1)
  for(i in 1:nrow(x)){
    ret[i] <- all(x[i,])
  }
  return(as.logical(ret))
}