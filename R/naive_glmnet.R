#' @title Naive glmnet fitting procedure
#' @description Naive glmnet fitting procedure
#' @param z1 A data matrix
#' @param z2 A data matrix
#' @param y1 A vector
#' @param y2 A vector
#' @param s Default to "lambda.min"
#' @param ... Extra parameter settings for cv.glmnet
#' @importFrom glmnet cv.glmnet
#' @importFrom glmnet coef.glmnet
#' @return A vector
#' @export
#' @examples
#' data(cpop_data_binary, package = 'CPOP')
#' attach(cpop_data_binary)
#' set.seed(1)
#' z1 = pairwise_col_diff(x1)
#' z2 = pairwise_col_diff(x2)
#' cpop_result = cpop_model(z1, z2, y1, y2, alpha = 1, n_features = 10)
#' lasso_result = naive_glmnet(z1, z2, y1, y2, alpha = 1, intercept = FALSE)
#' cpop_result
#' lasso_result
#' plot_cpop(cpop_result)
#' plot_cpop(lasso_result)
naive_glmnet = function(z1, z2, y1, y2, s = "lambda.min", ...){
  glmnet1 = glmnet::cv.glmnet(x = z1, y = y1, ...)
  glmnet2 = glmnet::cv.glmnet(x = z2, y = y2, ...)

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

  class(result) = c("cpop", class(result))
  return(result)
}

