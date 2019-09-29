#' @title A simulated binary data
#' @description A simulated binary data
#' @examples
#' \dontrun{
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
#' save(x1, x2, y1, y2, file = "./data/cpop_data_binary.RData",
#' compress = "xz")
#' }


#' @title A melanoma data
#' @description A melanoma data
#' @usage data(melanoma_example, package = 'CPOP')
