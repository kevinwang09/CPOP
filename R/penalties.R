#' @title Compute feature-wise absolute difference penalty
#' @description Compute feature-wise absolute difference penalty
#' @param x1 A data matrix
#' @param x2 A data matrix
#' @return A vector
#' @importFrom assertthat assert_that
#' @rdname penalties
#' @export
#' @examples
#' n = 20
#' p = 5
#' x1 = matrix(rnorm(n * p, mean = 0, sd = 1), nrow = n, ncol = p)
#' x2 = matrix(rnorm(n * p, mean = 1, sd = 1), nrow = n, ncol = p)
#' colmeans_penalty(x1, x2)
colmeans_penalty = function(x1, x2){

  assertthat::assert_that(
    identical(ncol(x1), ncol(x2))
  )

  m_x1 = colMeans(x1, na.rm = TRUE)
  m_x2 = colMeans(x2, na.rm = TRUE)

  p = ncol(x1)
  abs_penalty = p_transform(abs(m_x1 - m_x2))
  return(abs_penalty)
}

#' @title p-transform on penalty
#' @param penalty A vector of penalties to be transformed
#' where the sum is set to the length of the vector
#' @rdname penalties
#' @export
p_transform = function(penalty){
  p = length(penalty)
  new_penalty1 = ifelse(penalty <= 0, 0, penalty)
  if(sum(new_penalty1) > 0){
    return(new_penalty2 = (new_penalty1*p)/sum(new_penalty1))
  } else {
    return(rep(1, p))
  }
}

#' @title Compute t-stat difference penalty
#' @description Compute t-stat absolute difference penalty
#' @param x1 A data matrix
#' @param x2 A data matrix
#' @param y1 A factor response corresponding to the columns of x1
#' @param y2 A factor response corresponding to the columns of x2
#' @return A vector
#' @importFrom genefilter colttests
#' @importFrom assertthat assert_that
#' @rdname penalties
#' @export
#' @examples
#' n = 20
#' p = 5
#' x1 = matrix(rnorm(n * p, mean = 0, sd = 1), nrow = n, ncol = p)
#' y1 = factor(rbinom(n, 1, prob = 0.5), levels = c("0", "1"))
#' x2 = matrix(rnorm(n * p, mean = 1, sd = 1), nrow = n, ncol = p)
#' y2 = factor(rbinom(n, 1, prob = 0.5), levels = c("0", "1"))
#' tstat_penalty(x1 = x1, x2 = x2, y1 = y1, y2 = y2)
tstat_penalty = function(x1, x2, y1, y2){
  assertthat::assert_that(is.factor(y1))
  assertthat::assert_that(is.factor(y2))
  assertthat::assert_that(identical(levels(y1), levels(y2)))
  t1 = genefilter::colttests(x = x1, fac = y1, tstatOnly = TRUE, na.rm = TRUE)$statistic
  t2 = genefilter::colttests(x = x2, fac = y2, tstatOnly = TRUE, na.rm = TRUE)$statistic
  abs_penalty = p_transform(abs(t1 - t2))
  return(abs_penalty)
}


#' @title Compute KS-statistics penalty
#' @param x1 A data matrix
#' @param x2 A data matrix
#' @param statistic Should the KS-test statistic be returned (default) or the p-value should be
#' returned. Logical.
#' @return A vector of length matching that of x1 and x2
#' @importFrom purrr map map_dbl
#' @rdname penalties
#' @export
#' @examples
#' n = 20
#' p = 5
#' x1 = matrix(rnorm(n * p, mean = 0, sd = 1), nrow = n, ncol = p)
#' y1 = factor(rbinom(n, 1, prob = 0.5), levels = c("0", "1"))
#' x2 = matrix(rnorm(n * p, mean = 1, sd = 1), nrow = n, ncol = p)
#' y2 = factor(rbinom(n, 1, prob = 0.5), levels = c("0", "1"))
#' ks_penalty(x1 = x1, x2 = x2)
ks_penalty = function(x1, x2, statistic = TRUE){

  assertthat::assert_that(identical(ncol(x1), ncol(x2)))

  p = ncol(x1)
  ks_result = result = purrr::map(
    .x = seq_len(p),
    .f = ~ ks.test(x = x1[,.x], y = x2[,.x], alternative = "two.sided"))

  if(statistic){
    return(purrr::map_dbl(ks_result, "statistic"))
  } else {
    return(purrr::map_dbl(ks_result, "p.value"))
  }



}
