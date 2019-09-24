#' @title All three steps of CPOP modelling
#' @description All three steps of CPOP modelling
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
#' @param cpop2_break Should cpop2 loop be broken the first time
#' @param cpop2_type "sign" or "mag"
#' @param cpop2_mag a threshold
#' differential betas are removed
#' @param ... Extra parameter settings for cv.glmnet in cpop1
#' @importFrom glmnet cv.glmnet
#' @importFrom glmnet coef.cv.glmnet
#' @return A vector
#' @export
#' @examples
#' data(cpop_data_binary, package = 'CPOP')
#' set.seed(1)
#' z1 = pairwise_col_diff(x1)
#' z2 = pairwise_col_diff(x2)
#' w = compute_weights(z1, z2)
#' cpop_model_result = cpop_model(z1, z2, y1, y2, w = w,
#' alpha = 1, n_features = 40, s = "lambda.min")
#' alpha = c(1, 0.1)
#' cpop_model_result = cpop_model(z1, z2, y1, y2, w = w,
#' cpop1_iterate = TRUE, alpha = alpha, n_features = 40, s = "lambda.min",
#' family = "binomial")
cpop_model = function(z1, z2, y1, y2, w,
                     n_features = 50, nIter = 20, alpha = 1,
                     family = "binomial",
                     s = "lambda.min", cpop2_break = TRUE, cpop2_type = "sign", cpop2_mag = 1, ...){

  cpop1_result = cpop1_iterate(z1 = z1, z2 = z2, y1 = y1, y2 = y2, w = w,
                             n_features = n_features, nIter = nIter,
                             alpha = alpha, s = s,
                             family = family,
                             ...)
  if(length(cpop1_result) == 0){return(NULL)}
  if (cpop2_type == "sign"){
    cpop2_result = cpop2_sign(z1 = z1, z2 = z2, y1 = y1, y2 = y2,
                     cpop1_result = cpop1_result, s = s, nIter = nIter, family = family,
                     cpop2_break = cpop2_break)}
  if (cpop2_type == "mag"){
    cpop2_result = cpop2_mag(z1 = z1, z2 = z2, y1 = y1, y2 = y2,
                            cpop1_result = cpop1_result, s = s, nIter = nIter, family = family,
                            cpop2_break = FALSE, mag = cpop2_mag)}


  if(length(cpop2_result) == 0){return(NULL)}
  cpop3_result = cpop3(z1 = z1, z2 = z2, y1 = y1, y2 = y2,
                     cpop2_result = cpop2_result, family = family, intercept = FALSE)

  return(cpop3_result)
}
