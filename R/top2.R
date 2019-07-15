#' @title Step 2 of the TOP method
#' @description Step 2 of the TOP method
#' @param top1_result top1 result
#' @param z1 A data matrix
#' @param z2 A data matrix
#' @param y1 A vector
#' @param y2 A vector
#' @param nIter Number of iterations
#' @param s CV-Lasso lambda
#' @param ... Extra parameter settings for cv.glmnet
#' @param family see glmnet family
#' @param top2_break Should top2 loop be broken the first time
#' differential betas are removed
#' @importFrom glmnet cv.glmnet
#' @importFrom glmnet coef.cv.glmnet
#' @return A vector
#' @export
#' @examples
#' set.seed(1)
#' n = 1000
#' p = 10
#' x1 = matrix(rnorm(n * n, mean = 0, sd = 1), nrow = n, ncol = p)
#' x2 = matrix(rnorm(n * p, mean = 1, sd = 1), nrow = n, ncol = p)
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
#' top1_result = top1(z1, z2, y1, y2, w, nIter = 20, alpha = 1, s = "lambda.min")
#' s = "lambda.min"
#' top2_result = top2_sign(z1, z2, y1, y2, top1_result = top1_result, s = "lambda.min", nIter = 20)
top2_sign = function(z1, z2, y1, y2, top1_result, s = "lambda.min", nIter = 20,
                family = "binomial", top2_break = TRUE, ...){
  p = length(top1_result)
  top2_features = top1_result

  for(j in 1:nIter){
    z1_reduced = z1[,top2_features]
    z2_reduced = z2[,top2_features]

    ridge1 = glmnet::cv.glmnet(
      x = z1_reduced,
      y = y1,
      family = family,
      alpha = 0, ...)

    coef1 = glmnet::coef.cv.glmnet(ridge1, s = s)[-1, , drop = FALSE]
    signCoef1 = sign(coef1)

    ridge2 = glmnet::cv.glmnet(
      x = z2_reduced,
      y = y2,
      family = family,
      alpha = 0, ...)

    coef2 = glmnet::coef.cv.glmnet(ridge2, s = s)[-1, , drop = FALSE]
    signCoef2 = sign(coef2)

    top2_features = top2_features[as.matrix(signCoef1 == signCoef2)]

    confTable = table(
      factor(as.matrix(signCoef1), levels = c(-1, 0, 1)),
      factor(as.matrix(signCoef2), levels = c(-1, 0, 1)))
    confTable_diag0 = confTable
    diag(confTable_diag0) = 0
    message("TOP2 - Sign: Step ", sprintf("%02d", j), ": Number of leftover features: ", length(top2_features), " out of ", p)
    message("The sign matrix between the two data:")
    print(confTable_diag0)
    if(top2_break & sum(confTable_diag0) == 0){break}
  } ## End j-loop
  return(top2_features)
}
############################################
#' @title Step 2 of the TOP method
#' @description Step 2 of the TOP method
#' @param top1_result top1 result
#' @param z1 A data matrix
#' @param z2 A data matrix
#' @param y1 A vector
#' @param y2 A vector
#' @param nIter Number of iterations
#' @param s CV-Lasso lambda
#' @param ... Extra parameter settings for cv.glmnet
#' @param family see glmnet family
#' @param mag a threshold
#' differential betas are removed
#' @importFrom glmnet cv.glmnet
#' @importFrom glmnet coef.cv.glmnet
#' @return A vector
#' @export
#' @examples
#' set.seed(1)
#' n = 1000
#' p = 10
#' x1 = matrix(rnorm(n * n, mean = 0, sd = 1), nrow = n, ncol = p)
#' x2 = matrix(rnorm(n * p, mean = 1, sd = 1), nrow = n, ncol = p)
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
#' top1_result = top1(z1, z2, y1, y2, w, nIter = 20, alpha = 1, s = "lambda.min")
#' s = "lambda.min"
#' top2_result = top2_mag(z1, z2, y1, y2, top1_result = top1_result, s = "lambda.min", nIter = 20)
top2_mag = function(z1, z2, y1, y2, top1_result, s = "lambda.min", nIter = 20,
                family = "binomial", top2_break = FALSE, mag = 1, ...){
  p = length(top1_result)
  top2_features = top1_result

  for(j in 1:nIter){
    z1_reduced = z1[,top2_features]
    z2_reduced = z2[,top2_features]

    ridge1 = glmnet::cv.glmnet(
      x = z1_reduced,
      y = y1,
      family = family,
      alpha = 0, ...)

    coef1 = glmnet::coef.cv.glmnet(ridge1, s = s)[-1, , drop = FALSE]
    signCoef1 = sign(coef1)

    ridge2 = glmnet::cv.glmnet(
      x = z2_reduced,
      y = y2,
      family = family,
      alpha = 0, ...)

    coef2 = glmnet::coef.cv.glmnet(ridge2, s = s)[-1, , drop = FALSE]
    signCoef2 = sign(coef2)


    criterion = 0.5*abs(coef1 - coef2)/abs(coef1 + coef2)
    top2_features = top2_features[as.vector(criterion <= mag)]
    message("TOP2 - Mag: Step ", sprintf("%02d", j), ": Number of leftover features: ", length(top2_features), " out of ", p)


    if(top2_break){break}
  } ## End j-loop
  return(top2_features)
}
