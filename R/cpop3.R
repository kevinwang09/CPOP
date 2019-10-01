#' @title Step 3 of the CPOP method
#' @description Step 3 of the CPOP method
#' @param z1 A data matrix
#' @param z2 A data matrix
#' @param y1 A vector
#' @param y2 A vector
#' @param cpop2_result cpop2 result
#' @param intercept default to FALSE
#' @param ... Extra parameter settings for cv.glmnet
#' @param family see glmnet family
#' @importFrom glmnet cv.glmnet
#' @importFrom glmnet coef.cv.glmnet
#' @rdname cpop3
#' @return A vector
#' @export
#' @examples
#' data(cpop_data_binary, package = 'CPOP')
#' set.seed(1)
#' z1 = pairwise_col_diff(x1)
#' z2 = pairwise_col_diff(x2)
#' w = compute_weights(z1, z2)
#' cpop1_result = cpop1_iterate(z1 = z1, z2 = z2, y1 = y1, y2 = y2, w = w,
#'                              family = "binomial", alpha = 1)
#' cpop2_result = cpop2_sign(z1 = z1, z2 = z2, y1 = y1, y2 = y2,
#'                           cpop1_result = cpop1_result, family = "binomial")
#' cpop3_result = cpop3(z1 = z1, z2 = z2, y1 = y1, y2 = y2,
#'                      cpop2_result = cpop2_result, family = "binomial")
#' plot(predict_cpop(cpop3_result, newz = z1, model_number = 1),
#'      predict_cpop(cpop3_result, newz = z1, model_number = 2))
#' abline(a = 0, b = 1, col = "red")
cpop3 = function(z1, z2, y1, y2, cpop2_result, family, intercept = FALSE, ...){
  glmnet1 = glmnet::cv.glmnet(
      x = z1[,cpop2_result],
      y = y1,
      family = family,
      alpha = 0,
      intercept = intercept,
      ...)

  glmnet2 = glmnet::cv.glmnet(
      x = z2[,cpop2_result],
      y = y2,
      family = family,
      alpha = 0,
      intercept = intercept,
      ...)

  result = list(glmnet1 = glmnet1, glmnet2 = glmnet2, feature = cpop2_result)
  return(result)
}
