#' @title Compute feature-wise concordance weights
#' @description Compute feature-wise concordance weights
#' @param x1 A data matrix
#' @param x2 A data matrix
#' @return A vector
#' @export
#' @examples
#' n = 100
#' p = 10
#' x1 = matrix(rnorm(n * n, mean = 0, sd = 1), nrow = n, ncol = p)
#' x2 = matrix(rnorm(n * p, mean = 1, sd = 1), nrow = n, ncol = p)
#' compute_weights(x1, x2)

compute_weights = function(x1, x2){

  stopifnot({
    identical(ncol(x1), ncol(x2))
  })

  mX1 = colMeans(x1)
  mX2 = colMeans(x2)

  p = ncol(x1)
  absPenalty = pTransform(abs(mX1 - mX2))
  return(absPenalty)
}

pTransform = function(penalty){
  p = length(penalty)
  newPenalty1 = ifelse(penalty <= 0, 0, penalty)
  newPenalty2 = newPenalty1*p/sum(newPenalty1)
  return(newPenalty2)
}
