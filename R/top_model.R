#' @title All three steps of TOP modelling
#' @description All three steps of TOP modelling
#' @param z1 A data matrix
#' @param z2 A data matrix
#' @param y1 A vector
#' @param y2 A vector
#' @param w A vector
#' @param n_features n_features desired
#' @param nIter Number of iterations
#' @param alpha Lasso alpha
#' @param s CV-Lasso lambda
#' @param family family of glmnet
#' @param top2_break Should top2 loop be broken the first time
#' @param top2_type "sign" or "mag"
#' @param top2_mag a threshold
#' differential betas are removed
#' @param ... Extra parameter settings for cv.glmnet in top1
#' @importFrom glmnet cv.glmnet
#' @importFrom glmnet coef.cv.glmnet
#' @return A vector
#' @export
#' @examples
#' set.seed(1)
#' n = 100
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
#' top_model_result = top_model(z1, z2, y1, y2, w = w,
#' alpha = 1, n_features = 40, s = "lambda.min")
#' alpha = c(1, 0.1)
#' top_model_result = top_model(z1, z2, y1, y2, w = w,
#' top1_iterate = TRUE, alpha = alpha, n_features = 40, s = "lambda.min")
#' top_model_result2 = top_model(z1, z2, y1, y2, w = w,
#' top1_iterate = TRUE, alpha = alpha, n_features = 40, s = "lambda.min",
#' top2_type = "mag", top2_mag = 1)
#'
#' top_model_result = top_model(z1, z2, round(abs(y1)), round(abs(y2)), w = w,
#'                              top1_iterate = TRUE, alpha = alpha,
#'                              n_features = 40, s = "lambda.min",
#'                              family = "poisson")
top_model = function(z1, z2, y1, y2, w,
                     n_features = 50, nIter = 20, alpha = 1,
                     family = "binomial",
                     s = "lambda.min", top2_break = TRUE, top2_type = "sign", top2_mag = 1, ...){

  top1_result = top1_iterate(z1 = z1, z2 = z2, y1 = y1, y2 = y2, w = w,
                             n_features = n_features, nIter = nIter,
                             alpha = alpha, s = s,
                             family = family,
                             ...)
  if(length(top1_result) == 0){return(NULL)}
  if (top2_type == "sign"){
    top2_result = top2_sign(z1 = z1, z2 = z2, y1 = y1, y2 = y2,
                     top1_result = top1_result, s = s, nIter = nIter, family = family,
                     top2_break = top2_break)}
  if (top2_type == "mag"){
    top2_result = top2_mag(z1 = z1, z2 = z2, y1 = y1, y2 = y2,
                            top1_result = top1_result, s = s, nIter = nIter, family = family,
                            top2_break = FALSE, mag = top2_mag)}


  if(length(top2_result) == 0){return(NULL)}
  top3_result = top3(z1 = z1, z2 = z2, y1 = y1, y2 = y2,
                     top2_result = top2_result, family = family, intercept = FALSE)

  return(top3_result)
}
