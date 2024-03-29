#' @title CPOP internal functions
#' @description Step 2 of the CPOP method based on sign
#' @param cpop1_features cpop1 result
#' @param nIter Number of iterations
#' @param s CV-Lasso lambda
#' @param ... Extra parameter settings for cv.glmnet
#' @param family see glmnet family
#' @param cpop2_break Should cpop2 loop be broken the first time
#' @param intercept Intercept term
#' differential betas are removed
#' @rdname cpop_internals
#' @importFrom glmnet cv.glmnet
#' @importFrom glmnet coef.glmnet
#' @return A vector of features
#' @export
cpop2_sign = function(z1, z2, y1, y2, family, cpop1_features, s = "lambda.min", nIter = 20,
                cpop2_break = TRUE, intercept, ...){
  p = length(cpop1_features)

  if(p < 2){
    warning("Only one feature (", cpop1_features, ") can be selected by the CPOP. Classical glm results weill be given for this single feature model.")
    return(cpop1_features)
  }

  cpop2_features = cpop1_features

  for(j in 1:nIter){
    z1_reduced = z1[,cpop2_features, drop = FALSE]
    z2_reduced = z2[,cpop2_features, drop = FALSE]

    ridge1 = glmnet::cv.glmnet(
      x = z1_reduced,
      y = y1,
      family = family,
      alpha = 0, intercept = intercept, ...)

    coef1 = glmnet::coef.glmnet(ridge1, s = s)[-1, , drop = FALSE]
    signCoef1 = sign(coef1)

    ridge2 = glmnet::cv.glmnet(
      x = z2_reduced,
      y = y2,
      family = family,
      alpha = 0, intercept = intercept, ...)

    coef2 = glmnet::coef.glmnet(ridge2, s = s)[-1, , drop = FALSE]
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
    if(cpop2_break & sum(confTable_diag0) == 0){break} ## If all features with discorded signs are removed then break
    if(length(cpop2_features) == 0){
      warning("CPOP step 2 removed all features, CPOP 1 features will be used without any filtering")
      return(cpop1_features)
      }
  } ## End j-loop
  return(cpop2_features)
}
############################################
#' @title Step 2 of the CPOP method based on scaled magnitude
#' @description Step 2 of the CPOP method based on scaled magnitude
#' @param mag a scaled threshold differential betas are removed
#' @importFrom glmnet cv.glmnet
#' @importFrom glmnet coef.glmnet
#' @rdname cpop_internals
#' @export
cpop2_mag = function(z1, z2, y1, y2, family, cpop1_features, s = "lambda.min", nIter = 20,
                 cpop2_break = FALSE, mag = 1, intercept, ...){
  p = length(cpop1_features)
  cpop2_features = cpop1_features

  for(j in 1:nIter){
    if(length(cpop2_features) <= 10){
      break
    }
    z1_reduced = z1[,cpop2_features, drop = FALSE]
    z2_reduced = z2[,cpop2_features, drop = FALSE]

    ridge1 = glmnet::cv.glmnet(
      x = z1_reduced,
      y = y1,
      family = family,
      alpha = 0, intercept = intercept, ...)

    coef1 = glmnet::coef.glmnet(ridge1, s = s)[-1, , drop = FALSE]
    signCoef1 = sign(coef1)

    ridge2 = glmnet::cv.glmnet(
      x = z2_reduced,
      y = y2,
      family = family,
      alpha = 0, intercept = intercept, ...)

    coef2 = glmnet::coef.glmnet(ridge2, s = s)[-1, , drop = FALSE]
    signCoef2 = sign(coef2)

    criterion = (1/sqrt(2))*abs(coef1 - coef2)
    cpop2_features = cpop2_features[as.vector(criterion <= mag)]
    message("CPOP2 - Mag: Step ", sprintf("%02d", j), ": Number of leftover features: ", length(cpop2_features), " out of ", p)


    if(cpop2_break){break}
  } ## End j-loop
  return(cpop2_features)
}
