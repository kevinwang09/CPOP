#' @title A simulated binary data
#' @description A simulated binary data
#' @param x1 A data matrix
#' @param x2 A data matrix
#' @param y1 A vector
#' @param y2 A vector
#' @usage data(cpop_data, package = 'CPOP')
#' @examples
#' set.seed(1)
#' n = 1000
#' p = 10
#' x1 = matrix(rnorm(n * p, mean = 0, sd = 1), nrow = n, ncol = p)
#' x2 = x1 + 0.1
#' colnames(x1) = colnames(x2) = paste0("X", 1:p)
#' k = 2
#' beta = c(rep(1, k), rep(0, p - k))
#' expit = function(x) 1/(1+exp(-x))
#' y1 = rbinom(n, 1, prob = expit(x1 %*% beta))
#' y2 = rbinom(n, 1, prob = expit(x2 %*% beta))
#' save(x1, x2, y1, y2, file = "./data/cpop_data.RData")


#' @title A melanoma data
#' @description A melanoma data
#' @param list_melanoma_means list_melanoma_means
#' @param list_melanoma_sigma list_melanoma_sigma
#' @param list_melanoma_samples list_melanoma_samples
#' @usage data(melanoma_example, package = 'CPOP')
