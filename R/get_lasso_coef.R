#' @title Get cv.glmnet coefficient as a matrix or a tibble
#' @description Get cv.glmnet coefficient as a matrix or a tibble
#' @param lassoObj cv.glmnet object
#' @param s CV-Lasso lambda
#' @param tibble Default to FALSE
#' @importFrom glmnet coef.cv.glmnet
#' @return A matrix or a tibble
#' @export
#' @examples
#' set.seed(1)
#' n = 1000
#' p = 10
#' x = matrix(rnorm(n * p, mean = 0, sd = 1), nrow = n, ncol = p)
#' colnames(x) = paste0("X", 1:p)
#' k = 2
#' beta = c(rep(1, k), rep(0, p - k))
#' expit = function(x) 1/(1+exp(-x))
#' y = rbinom(n, 1, prob = expit(x %*% beta))
#' lassoObj = glmnet::cv.glmnet(x = x, y = y, family = "binomial")
#' get_lasso_coef(lassoObj = lassoObj, s = "lambda.min")
#' get_lasso_coef(lassoObj = lassoObj, s = "lambda.min", tibble = TRUE)

get_lasso_coef = function(lassoObj, s, tibble = FALSE){
  if("cv.glmnet" %in% class(lassoObj)){
    coef_matrix = as.matrix(glmnet::coef.cv.glmnet(lassoObj, s = s))
  } else if("glmnet" %in% class(lassoObj)){
    coef_matrix = as.matrix(glmnet::coef.glmnet(lassoObj, s = s))
  } else {
    stop("Only glmnet and cv.glmnet objects are acceptable")
  }

  result = coef_matrix[coef_matrix[, 1] != 0, , drop = FALSE]

  if(tibble){
    result = tibble::tibble(
      feature_name = rownames(result),
      beta = result[,1])
  }


  return(result)
}
