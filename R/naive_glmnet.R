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
#' naive_glmnet_result$coef_tibble %>%
#' ggplot(aes(x = coef1, y = coef2, label = feature_name)) +
#' geom_text() +
#' geom_abline(slope = 1, intercept = 0, colour = "red")

naive_glmnet = function(z1, z2, y1, y2, s = "lambda.min", ...){
  glmnet1 = glmnet::cv.glmnet(x = z1, y = y1, ...)
  glmnet2 = glmnet::cv.glmnet(x = z2, y = y2, ...)

  coef1 = glmnet::coef.cv.glmnet(glmnet1, s = s)
  coef2 = glmnet::coef.cv.glmnet(glmnet2, s = s)

  coef_tibble = tibble::tibble(
    feature_name = rownames(coef1),
    coef1 = as.vector(coef1),
    coef2 = as.vector(coef2)
  )


  result = list(glmnet1 = glmnet1,
                glmnet2 = glmnet2,
                coef_tibble = coef_tibble)
  return(result)
}
