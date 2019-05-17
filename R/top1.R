#' @title Step 1 of the TOP method
#' @description Step 1 of the TOP method
#' @param z1 A data matrix
#' @param z2 A data matrix
#' @param y1 A vector
#' @param y2 A vector
#' @param w A vector
#' @param nIter Number of iterations
#' @param alpha Lasso alpha
#' @param s CV-Lasso lambda
#' @param ... Extra parameter settings for cv.glmnet
#' @importFrom glmnet cv.glmnet
#' @importFrom glmnet coef.cv.glmnet
#' @return A vector
#' @export
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
#' z1 = pairwise_col_diff(x1)
#' z2 = pairwise_col_diff(x2)
#' w = compute_weights(z1, z2)
#' nIter = 20
#' top1(z1, z2, y1, y2, w, nIter = 20, alpha = 1, s = "lambda.min")
top1 = function(z1, z2, y1, y2, w, nIter = 20, alpha = 1, s = "lambda.min", ...){
  remaining_features = colnames(z1)

  for(i in 1:nIter){
    print(length(remaining_features))

    en1 = glmnet::cv.glmnet(
      x = z1[,remaining_features],
      y = y1,
      family = "binomial",
      penalty.factor = w[remaining_features],
      ...)

    en2 = glmnet::cv.glmnet(
      x = z2[,remaining_features],
      y = y2,
      family = "binomial",
      penalty.factor = w[remaining_features],
      ...)

    remaining_features = setdiff(
      remaining_features, base::intersect(
        rownames(get_lasso_coef(en1, s = s)),
        rownames(get_lasso_coef(en2, s = s)))
    )
  } ## End j-loop

  common_features = setdiff(colnames(z1), remaining_features)
  return(common_features)
}



get_lasso_coef = function(lassoObj, s){
  coefMatrix = as.matrix(glmnet::coef.cv.glmnet(lassoObj, s = s))
  result = coefMatrix[coefMatrix[, 1] != 0, , drop = FALSE]
  return(result)
}
