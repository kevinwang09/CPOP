#' @title CPOP modelling
#' @description CPOP is consisted of three steps. Step 1 is to select features common to
#' two transformed data. Note the input must be pairwise-differences between the original data columns.
#' Step 2 is to select features in constructed models that shared similar characteristics. Step 3 is to
#' construct a final model used for prediction.
#' @param x1 A data matrix of size n (number of samples) times p (number of features)
#' @param x2 A data matrix of size n (number of samples) times p (number of features)
#' Column names should be identical to z1.
#' @param y1 A vector of response variable. Same length as the number of rows of x1.
#' @param y2 A vector of response variable. Same length as the number of rows of x2.
#' @param w A vector of weights. Default to NULL, which uses `identity_dist`.
#' @param n_iter Number of iterations in Step 1 and 2. Default to 20.
#' @param alpha The alpha parameter for elastic net models. See the alpha argument in glmnet::glmnet. Default to 1.
#' @param n_features Breaking the CPOP-Step 1 loop if a certain number of features is reached. Default to 50.
#' @param s CV-Lasso lambda choice. Default to "lambda.min", see cv.glmnet in the glmnet package.
#' @param family family of glmnet
#' @param cpop1_method CPOP step 1 selection method. See documentations on `cpop1`. Default to "Normal".
#' @param cpop2_break Should CPOP-step2 loop be broken the first time. Default to TRUE.
#' @param cpop2_type Should CPOP-step2 select features based on sign of features of magnitude? Either "sign" (default) or "mag"..
#' @param cpop2_mag a threshold for CPOP-step2 when selecting features based on coefficient difference magnitude.
#' differential betas are removed
#' @param intercept Default to FALSE
#' @param ... Extra parameter settings for cv.glmnet in in the glmnet package.
#' @param z1 (Deprecated) a data matrix, columns are pairwise-differences between the original data columns.
#' @param z2 (Deprecated) a data matrix, columns are pairwise-differences between the original data columns.
#' @return A CPOP object containing:
#' \itemize{
#' \item model: the CPOP model as a glmnet object
#' \item coef_tbl: a tibble (data frame) of CPOP feature coefficients
#' \item cpop1_features: a vector of CPOP
#' }
#' @export
#' @examples
#' data(cpop_data_binary, package = 'CPOP')
#' ## Loading simulated matrices and vectors
#' x1 = cpop_data_binary$x1
#' x2 = cpop_data_binary$x2
#' y1 = cpop_data_binary$y1
#' y2 = cpop_data_binary$y2
#' set.seed(1)
#' cpop_result = cpop_model(x1 = x1, x2 = x2, y1 = y1, y2 = y2, alpha = 1, n_features = 10)
#' cpop_result
cpop_model <- function(
    x1, x2, y1, y2, w = NULL,
    n_features = 50, n_iter = 20, alpha = 1,
    family = "binomial",
    s = "lambda.min", cpop2_break = TRUE, cpop2_type = "sign", cpop2_mag = 1,
    cpop1_method = "normal", intercept = FALSE, z1, z2, ...){

  ## Checking input models
  if(missing(z1) | missing(z2)){

    prep_result = prep_cpop(x1 = x1, x2 = x2)
    z1 = prep_result$z1
    z2 = prep_result$z2

    assertthat::assert_that(nrow(x1) == length(y1))
    assertthat::assert_that(nrow(x2) == length(y2))
  }

  if(missing(x1) | missing(x2)){

    warning(
      "Arguments `z1` and `z2` are deprecated. CPOP can still be performed.
    Please use `x1` and `x2` in the future.")

    assertthat::assert_that(nrow(z1) == length(y1))
    assertthat::assert_that(nrow(z2) == length(y2))
  }

  ## Checking binomial inputs
  if(family == "binomial"){
    assertthat::assert_that(is.factor(y1))
    assertthat::assert_that(is.factor(y2))
    assertthat::assert_that(identical(levels(y1), levels(y2)))
    factor_levels = levels(y1)
  } else {
    factor_levels = NULL
  }

  cpop1_result = cpop1_iterate(
    z1 = z1, z2 = z2, y1 = y1, y2 = y2, w = w,
    n_features = n_features, n_iter = n_iter,
    alpha = alpha, s = s,
    family = family, cpop1_method = cpop1_method, ...)

  cpop1_features = cpop1_result$cpop1_features

  if(length(cpop1_features) == 0){
    warning("No predictive features were selected in Step 1. Return NULL.")
    return(NULL)
    }

  if (cpop2_type == "sign"){
    cpop2_result = cpop2_sign(z1 = z1, z2 = z2, y1 = y1, y2 = y2,
                              cpop1_features = cpop1_features, s = s, n_iter = n_iter, family = family,
                              cpop2_break = cpop2_break, intercept = intercept)}
  if (cpop2_type == "mag"){
    cpop2_result = cpop2_mag(z1 = z1, z2 = z2, y1 = y1, y2 = y2,
                             cpop1_features = cpop1_features, s = s, n_iter = n_iter, family = family,
                             cpop2_break = FALSE, mag = cpop2_mag, intercept = intercept)}


  if(length(cpop2_result) == 0){
    warning("No predictive features were selected in Step 2. Return NULL.")
    return(NULL)
  }

  cpop3_result = cpop3(z1 = z1, z2 = z2, y1 = y1, y2 = y2,
                       cpop2_result = cpop2_result, family = family, intercept = intercept)

  coef1 = glmnet::coef.glmnet(cpop3_result$glmnet1, s = s)
  coef2 = glmnet::coef.glmnet(cpop3_result$glmnet2, s = s)
  coef_tbl = tibble::tibble(coef_name = rownames(coef1),
                               coef1 = as.vector(coef1),
                               coef2 = as.vector(coef2))

  result = c(cpop3_result,
             coef_tbl = list(coef_tbl),
             cpop1_features = list(cpop1_features),
             step_features = list(cpop1_result$step_features),
             family_params = list(list(family = family,
                                       factor_levels = factor_levels)),
             z1 = list(z1), z2 = list(z2))

  # class(result) = c("cpop", class(result))
  class(result) = c("cpop")
  return(result)
}

#' @export
print.cpop = function(x,...){
  cat("CPOP model with ", length(x$feature), "features \n")
  print(x$coef_tbl)
}
