#' @title Naive glmnet fitting procedure
#' @description Naive glmnet fitting procedure
#' @param z1 A data matrix
#' @param z2 A data matrix
#' @param y1 A vector
#' @param y2 A vector
#' @param s Default to "lambda.min"
#' @param ... Extra parameter settings for cv.glmnet
#' @importFrom glmnet cv.glmnet
#' @importFrom glmnet coef.cv.glmnet
#' @return A vector
#' @export
#' @examples
#' data(cpop_data_binary, package = 'CPOP')
#' set.seed(1)
#' z1 = pairwise_col_diff(x1)
#' z2 = pairwise_col_diff(x2)
#' s = "lambda.min"
#' naive_glmnet_result = naive_glmnet(z1, z2, y1, y2,
#' family = "binomial", alpha = 1, s = "lambda.min")
#' plot_glmnet_coef(naive_glmnet_result, s = "lambda.min", type = "point")
#' plot_glmnet_coef(naive_glmnet_result, s = "lambda.min", type = "text")
#' plot_glmnet_coef(naive_glmnet_result, s = "lambda.min", type = "bar")

naive_glmnet = function(z1, z2, y1, y2, s = "lambda.min", ...){
  glmnet1 = glmnet::cv.glmnet(x = z1, y = y1, ...)
  glmnet2 = glmnet::cv.glmnet(x = z2, y = y2, ...)

  result = list(glmnet1 = glmnet1,
                glmnet2 = glmnet2)
  return(result)
}
