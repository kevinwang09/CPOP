#' @title A simulated linear data
#' @description A simulated linear regression data
#' @param x1 A data matrix
#' @param x2 A data matrix
#' @param y1 A vector
#' @param y2 A vector
#' @usage data(cpop_data_linear, package = 'CPOP')
#' @examples
#' set.seed(1)
#' n = 1000
#' p = 10
#' x1 = matrix(rnorm(n * p, mean = 0, sd = 1), nrow = n, ncol = p)
#' x2 = x1 + matrix(rnorm(n * p, mean = 0, sd = 0.1), nrow = n, ncol = p)
#' colnames(x1) = colnames(x2) = sprintf("X%02d", 1:p)
#' k = 2
#' beta = c(rep(1, k), rep(0, p - k))
#' y1 = x1 %*% beta + rnorm(n, mean = 0, sd = 0.5)
#' y2 = x2 %*% beta + rnorm(n, mean = 0, sd = 0.5)
#' save(x1, x2, y1, y2, file = "./data/cpop_data_linear.RData")


#' @title A simulated binary data
#' @description A simulated binary data
#' @param x1 A data matrix
#' @param x2 A data matrix
#' @param y1 A vector
#' @param y2 A vector
#' @usage data(cpop_data_binary, package = 'CPOP')
#' @examples
#' set.seed(1)
#' n = 1000
#' p = 10
#' x1 = matrix(rnorm(n * p, mean = 0, sd = 1), nrow = n, ncol = p)
#' x2 = x1 + 0.1
#' colnames(x1) = colnames(x2) = paste0("X", 1:p)
#' k = 2
#' beta = c(rep(1, k), rep(0, p - k))
#' y1 = rbinom(n, 1, prob = CPOP::expit(x1 %*% beta))
#' y2 = rbinom(n, 1, prob = CPOP::expit(x2 %*% beta))
#' save(x1, x2, y1, y2, file = "./data/cpop_data_binary.RData")


#' @title A melanoma data
#' @description A melanoma data
#' @param list_melanoma_means list_melanoma_means
#' @param list_melanoma_sigma list_melanoma_sigma
#' @param list_melanoma_samples list_melanoma_samples
#' @usage data(melanoma_example, package = 'CPOP')
