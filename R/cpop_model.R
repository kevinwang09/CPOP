#' @title All three steps of CPOP modelling
#' @description All three steps of CPOP modelling
#' @param z1 A data matrix
#' @param z2 A data matrix
#' @param y1 A vector
#' @param y2 A vector
#' @param w A vector
#' @param n_features n_features desired
#' @param n_iter Number of iterations
#' @param alpha Lasso alpha
#' @param s CV-Lasso lambda
#' @param family family of glmnet
#' @param cpop2_break Should cpop2 loop be broken the first time
#' @param cpop2_type "sign" or "mag"
#' @param cpop2_mag a threshold
#' @param cpop1_method Default value is "normal". Alternatives are "after" and "either".
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
    warning("No predictive features were selected. Return NULL.")
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


  if(length(cpop2_result) == 0){return(NULL)}
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

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
