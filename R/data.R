#' @title A simulated binary data
#' @docType data
#' @description A simulated binary data
#' @format A data frame with columns:
#' \describe{
#'  \item{x1}{A matrix of size 100*20, each column has mean 1 and sd 1}
#'  \item{x2}{A matrix of size 100*20, each column has mean 2 and sd 1}
#'  \item{x3}{A matrix of size 100*20, each column has mean 3 and sd 1}
#'  \item{y1}{A vector of 0's and 1's, created by beta and x1}
#'  \item{y2}{A vector of 0's and 1's, created by beta and x2}
#'  \item{y3}{A vector of 0's and 1's, created by beta and x3}
#'  \item{beta}{A random vector with first 10 entries drawn from random unif(-1, 1), otherwise 0's.}
#' }
#' @examples
#' data(cpop_data_binary)
#' attach(cpop_data_binary)
#' \dontrun{
#' set.seed(13)
#' n = 100
#' p = 20
#' x1 = matrix(rnorm(n * p, mean = 1, sd = 1), nrow = n, ncol = p)
#' x2 = matrix(rnorm(n * p, mean = 2, sd = 1), nrow = n, ncol = p)
#' x3 = matrix(rnorm(n * p, mean = 3, sd = 1), nrow = n, ncol = p)
#' colnames(x1) = colnames(x2) = colnames(x3) = sprintf("X%02d", 1:p)
#' z1 = pairwise_col_diff(x1)
#' z2 = pairwise_col_diff(x2)
#' z3 = pairwise_col_diff(x3)
#' k = 10
#' q = choose(p, 2)
#' beta = c(runif(k, -1, 1), rep(0, q - k))
#' names(beta) = colnames(z1)
#' y1 = rbinom(n, 1, prob = CPOP::expit(z1 %*% beta))
#' y2 = rbinom(n, 1, prob = CPOP::expit(z2 %*% beta))
#' y3 = rbinom(n, 1, prob = CPOP::expit(z3 %*% beta))
#' cpop_data_binary = tibble::lst(x1, x2, x3, y1, y2, y3, beta)
#' usethis::use_data(cpop_data_binary)
#' }
"cpop_data_binary"

