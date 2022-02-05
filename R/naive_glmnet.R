#' @title Naive glmnet fitting procedure
#' @description Naive glmnet fitting procedure
#' @param z1 A data matrix
#' @param z2 A data matrix
#' @param y1 A vector
#' @param y2 A vector
#' @param family family of glmnet
#' @param s Default to "lambda.min"
#' @param ... Extra parameter settings for cv.glmnet
#' @importFrom glmnet cv.glmnet
#' @importFrom glmnet coef.glmnet
#' @return A vector
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
#' lasso_result = naive_glmnet(x1 = x1, x2 = x2, y1 = y1, y2 = y2, alpha = 1, intercept = FALSE)
#' cpop_result
#' lasso_result
#' plot_cpop(cpop_result)
#' plot_cpop(lasso_result)
#' z1 = pairwise_col_diff(x1)
#' z2 = pairwise_col_diff(x2)
#' plot(predict_cpop(cpop_result, newz = z1)$cpop_model_avg,
#' predict_naive_glmnet(lasso_result, newz = z1)$naive_glmnet_avg)
#' abline(a = 0, b = 1, col = "red")
naive_glmnet = function(x1, x2, y1, y2, s = "lambda.min", family = "binomial", z1, z2, ...){

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

  if(any(grepl("--", colnames(x1))) | any(grepl("--", colnames(x2)))){
    warning(
      "The arguments of CPOP has changed significantly in v0.1.0.
    It is likely that the inputs are still using `z1` and `z2`,
    which are now deprecated.")
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

  glmnet1 = glmnet::cv.glmnet(x = z1, y = y1, family = family, ...)
  glmnet2 = glmnet::cv.glmnet(x = z2, y = y2, family = family, ...)

  result = list(glmnet1 = glmnet1,
                glmnet2 = glmnet2)

  coef1 = glmnet::coef.glmnet(glmnet1, s = s)
  coef2 = glmnet::coef.glmnet(glmnet2, s = s)
  coef_tbl = tibble::tibble(coef_name = rownames(coef1),
                            coef1 = as.vector(coef1),
                            coef2 = as.vector(coef2))

  result = list(glmnet1 = glmnet1,
                glmnet2 = glmnet2,
                coef_tbl = list(coef_tbl))

  class(result) = c("naive_glmnet", class(result))
  return(result)
}

#' @title Prediction method for naive glmnet
#' @param glmnet_result glmnet_result
#' @param newz matrix
#' @param s Default to "lambda.min"
#' @export
#' @import glmnet
#' @importFrom tibble as_tibble
#' @importFrom tibble as_tibble
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr everything
predict_naive_glmnet = function(glmnet_result, newz, s = "lambda.min"){
  result1 = predict(object = glmnet_result$glmnet1, newx = newz, s = s)
  result2 = predict(object = glmnet_result$glmnet2, newx = newz, s = s)

  as.numeric((result1 + result2)/2)

  result_mat = cbind(result1, result2, (result1 + result2)/2)
  colnames(result_mat) = c("naive_glmnet1", "naive_glmnet2", "naive_glmnet_avg")

  if(is.null(rownames(result_mat))){
    rownames(result_mat) = 1:nrow(result_mat)
  }

  tib_result = tibble::as_tibble(data.frame(result_mat))
  tib_result = dplyr::mutate(tib_result, samples = rownames(result_mat))
  tib_result = dplyr::select(tib_result, samples, dplyr::everything())

return(tib_result)
}
