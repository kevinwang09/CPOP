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
#' @param cpop1_step_features default to NULL
#' differential betas are removed
#' @param intercept Default to FALSE
#' @param ... Extra parameter settings for cv.glmnet in cpop1
#' @return A vector
#' @export
#' @examples
#' data(cpop_data_binary, package = 'CPOP')
#' set.seed(1)
#' z1 = pairwise_col_diff(x1)
#' z2 = pairwise_col_diff(x2)
#' cpop_model_result = cpop_model(z1, z2, y1, y2, alpha = 1, n_features = 40, s = "lambda.min")
#' cpop_model_result$feature
#' cpop_model_result = cpop_model(z1, z2, y1, y2, alpha = 1, n_features = 40, s = "lambda.min", cpop1_step_features = "both")
cpop_model = function(z1, z2, y1, y2, w = NULL,
                      n_features = 50, n_iter = 20, alpha = 1,
                      family = "binomial",
                      s = "lambda.min", cpop2_break = TRUE, cpop2_type = "sign", cpop2_mag = 1,
                      cpop1_step_features = NULL, intercept = FALSE, ...){

  cpop1_result = cpop1_iterate(z1 = z1, z2 = z2, y1 = y1, y2 = y2, w = w,
                               n_features = n_features, n_iter = n_iter,
                               alpha = alpha, s = s,
                               family = family, ...)

  if(length(cpop1_result$cpop1_features) != 0){
    cpop1_features = cpop1_result$cpop1_features
  } else if(cpop1_step_features == "both"){
    warning("No predictive features commonly predictive in both data (at each iteration) were found \n alternative feature set was be used")
    message("Features ever selected by both data (after all iterations) will now be pooled")
    cpop1_features = cpop1_result$step_features %>%
      dplyr::filter(feature_name != "(Intercept)") %>%
      dplyr::select(coef_model, feature_name) %>%
      dplyr::distinct(coef_model, feature_name) %>%
      dplyr::group_by(feature_name) %>%
      dplyr::tally() %>%
      dplyr::filter(n > 1) %>%
      dplyr::pull(feature_name)
  } else if(cpop1_step_features == "either"){
      warning("No predictive features commonly predictive in both data (at each iteration) were found \n alternative feature set was be used")
      message("Features ever selected by either data will now be pooled")
      cpop1_features = cpop1_result$step_features %>%
        dplyr::filter(feature_name != "(Intercept)") %>%
        dplyr::pull(feature_name) %>% unique()
    }

  if(length(cpop1_features) == 0){return(NULL)}

  if (cpop2_type == "sign"){
    cpop2_result = cpop2_sign(z1 = z1, z2 = z2, y1 = y1, y2 = y2,
                              cpop1_features = cpop1_features, s = s, n_iter = n_iter, family = family,
                              cpop2_break = cpop2_break)}
  if (cpop2_type == "mag"){
    cpop2_result = cpop2_mag(z1 = z1, z2 = z2, y1 = y1, y2 = y2,
                             cpop1_features = cpop1_features, s = s, n_iter = n_iter, family = family,
                             cpop2_break = FALSE, mag = cpop2_mag)}


  if(length(cpop2_result) == 0){return(NULL)}
  cpop3_result = cpop3(z1 = z1, z2 = z2, y1 = y1, y2 = y2,
                       cpop2_result = cpop2_result, family = family, intercept = intercept)

  return(c(cpop3_result,
           cpop1_features = list(cpop1_features),
           step_features = list(cpop1_result$step_features)
  ))
}
