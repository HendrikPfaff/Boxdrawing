#' Repmat
#' 
#' R equivalent of the matlab function repmat. Repeats a matrix column- and rowwise.
#' 
#' @param X Matrix to be repeated.
#' @param m Number of repetitions  columnwise.
#' @param n Number of repititions rowwise.
#' @return New Matrix with repeated values.
repmat <- function(X, m, n){
  Y <- do.call(rbind, rep(list(X), m))
  do.call(cbind, rep(list(Y), n))
}