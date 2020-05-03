#' @title Compute feature-wise concordance penalty
#' @description Compute feature-wise concordance penalty
#' @param x1 A data matrix
#' @param x2 A data matrix
#' @return A vector
#' @importFrom assertthat assert_that
#' @rdname penalties
#' @export
#' @examples
#' n = 10
#' p = 10
#' x1 = matrix(rnorm(n * p, mean = 0, sd = 1), nrow = n, ncol = p)
#' x2 = matrix(rnorm(n * p, mean = 1, sd = 1), nrow = n, ncol = p)
#' colmeans_penalty(x1, x2)
colmeans_penalty = function(x1, x2){

  assertthat::assert_that(
    identical(ncol(x1), ncol(x2))
  )

  m_x1 = colMeans(x1)
  m_x2 = colMeans(x2)

  p = ncol(x1)
  abs_penalty = p_transform(abs(m_x1 - m_x2))
  return(abs_penalty)
}


#' @title p-transform on penalty
#' @param penalty A vector of penalties to be transformed
#' @rdname penalties
#' @export
p_transform = function(penalty){
  p = length(penalty)
  new_penalty1 = ifelse(penalty <= 0, 0, penalty)
  new_penalty2 = new_penalty1*p/sum(new_penalty1)
  return(new_penalty2)
}
