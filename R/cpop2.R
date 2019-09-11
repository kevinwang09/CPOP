#' @title Step 2 of the CPOP method
#' @description Step 2 of the CPOP method
#' @param cpop1_result cpop1 result
#' @param z1 A data matrix
#' @param z2 A data matrix
#' @param y1 A vector
#' @param y2 A vector
#' @param nIter Number of iterations
#' @param s CV-Lasso lambda
#' @param ... Extra parameter settings for cv.glmnet
#' @param family see glmnet family
#' @param cpop2_break Should cpop2 loop be broken the first time
#' differential betas are removed
#' @importFrom glmnet cv.glmnet
#' @importFrom glmnet coef.cv.glmnet
#' @return A vector
#' @export
#' @examples
#' data(cpop_data, package = 'CPOP')
#' set.seed(1)
#' z1 = pairwise_col_diff(x1)
#' z2 = pairwise_col_diff(x2)
#' w = compute_weights(z1, z2)
#' alpha = c(1, 0.1, 0.01)
#' cpop1_result = cpop1_iterate(z1, z2, y1, y2, w, nIter = 20,
#' alpha = alpha, n_features = 30, s = "lambda.min")
#' cpop2_result = cpop2_sign(z1, z2, y1, y2, cpop1_result = cpop1_result, s = "lambda.min", nIter = 20)
cpop2_sign = function(z1, z2, y1, y2, cpop1_result, s = "lambda.min", nIter = 20,
                family = "binomial", cpop2_break = TRUE, ...){
  p = length(cpop1_result)
  cpop2_features = cpop1_result

  for(j in 1:nIter){
    z1_reduced = z1[,cpop2_features]
    z2_reduced = z2[,cpop2_features]

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

    cpop2_features = cpop2_features[as.matrix(signCoef1 == signCoef2)]

    confTable = table(
      factor(as.matrix(signCoef1), levels = c(-1, 0, 1)),
      factor(as.matrix(signCoef2), levels = c(-1, 0, 1)))
    confTable_diag0 = confTable
    diag(confTable_diag0) = 0
    message("CPOP2 - Sign: Step ", sprintf("%02d", j), ": Number of leftover features: ", length(cpop2_features), " out of ", p)
    message("The sign matrix between the two data:")
    print(confTable_diag0)
    if(cpop2_break & sum(confTable_diag0) == 0){break}
  } ## End j-loop
  return(cpop2_features)
}
############################################
#' @title Step 2 of the CPOP method
#' @description Step 2 of the CPOP method
#' @param cpop1_result cpop1 result
#' @param z1 A data matrix
#' @param z2 A data matrix
#' @param y1 A vector
#' @param y2 A vector
#' @param nIter Number of iterations
#' @param s CV-Lasso lambda
#' @param ... Extra parameter settings for cv.glmnet
#' @param family see glmnet family
#' @param cpop2_break If break is needed, logical
#' @param mag a threshold
#' differential betas are removed
#' @importFrom glmnet cv.glmnet
#' @importFrom glmnet coef.cv.glmnet
#' @return A vector
#' @export
#' @examples
#' data(cpop_data, package = 'CPOP')
#' z1 = pairwise_col_diff(x1)
#' z2 = pairwise_col_diff(x2)
#' w = compute_weights(z1, z2)
#' alpha = c(1, 0.1, 0.01)
#' cpop1_result = cpop1_iterate(z1, z2, y1, y2, w, nIter = 20,
#' alpha = alpha, n_features = 30, s = "lambda.min")
#' cpop2_result = cpop2_mag(z1, z2, y1, y2, cpop1_result = cpop1_result, s = "lambda.min", nIter = 20)
cpop2_mag = function(z1, z2, y1, y2, cpop1_result, s = "lambda.min", nIter = 20,
                family = "binomial", cpop2_break = FALSE, mag = 1, ...){
  p = length(cpop1_result)
  cpop2_features = cpop1_result

  for(j in 1:nIter){
    z1_reduced = z1[,cpop2_features]
    z2_reduced = z2[,cpop2_features]

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
    cpop2_features = cpop2_features[as.vector(criterion <= mag)]
    message("CPOP2 - Mag: Step ", sprintf("%02d", j), ": Number of leftover features: ", length(cpop2_features), " out of ", p)


    if(cpop2_break){break}
  } ## End j-loop
  return(cpop2_features)
}
