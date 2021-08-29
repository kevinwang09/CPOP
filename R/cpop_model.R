#' @title CPOP modelling
#' @description CPOP is consisted of three steps. Step 1 is to select features common to
#' two transformed data. Note the input must be pairwise-differences between the original data columns.
#' Step 2 is to select features in constructed models that shared similar characteristics. Step 3 is to
#' construct a final model used for prediction.
#' @param z1 A data matrix, columns are pairwise-differences between the original data columns.
#' @param z2 A data matrix, columns are pairwise-differences between the original data columns.
#' Column names should be identical to z1.
#' @param y1 A vector of response variable. Same length as the number of rows of z1.
#' @param y2 A vector of response variable. Same length as the number of rows of z2.
#' @param w A vector of weights.
#' @param n_iter Number of iterations in Step 1 and 2.
#' @param alpha The alpha parameter for elastic net models. See the alpha argument in glmnet::glmnet.
#' @param n_features Breaking the CPOP-Step 1 loop if a certain number of features is reached.
#' @param s CV-Lasso lambda
#' @param family family of glmnet
#' @param cpop2_break Should cpop2 loop be broken the first time
#' @param cpop2_type "sign" or "mag"
#' @param cpop2_mag a threshold
#' differential betas are removed
#' @param intercept Default to FALSE
#' @param ... Extra parameter settings for cv.glmnet in cpop1
#' @return A vector
#' @export
#' @examples
#' data(cpop_data_binary, package = 'CPOP')
#' attach(cpop_data_binary)
#' set.seed(1)
#' z1 = pairwise_col_diff(x1)
#' z2 = pairwise_col_diff(x2)
#' cpop_result = cpop_model(z1, z2, y1, y2, alpha = 1, n_features = 10)
#' cpop_result
cpop_model <- function(z1, z2, y1, y2, w = NULL,
                      n_features = 50, n_iter = 20, alpha = 1,
                      family = "binomial",
                      s = "lambda.min", cpop2_break = TRUE, cpop2_type = "sign", cpop2_mag = 1,
                      cpop1_method = "normal", intercept = FALSE, ...){

  cpop1_result = cpop1_iterate(z1 = z1, z2 = z2, y1 = y1, y2 = y2, w = w,
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
             step_features = list(cpop1_result$step_features))

  class(result) = c("cpop", class(result))
  return(result)
}

cpop <- function(x, ...) UseMethod("cpop")

print.cpop <- function(cpop_result, ...)
{
  cat("CPOP model with ", length(cpop_result$feature), "features \n")
  print(cpop_result$coef_tbl)
}
