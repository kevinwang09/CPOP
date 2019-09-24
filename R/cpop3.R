#' @title Step 3 of the CPOP method iterate
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
#' @return A vector
#' @export
#' @examples
#' data(cpop_data_binary, package = 'CPOP')
#' set.seed(1)
#' z1 = pairwise_col_diff(x1)
#' z2 = pairwise_col_diff(x2)
#' w = compute_weights(z1, z2)
#' alpha = c(1, 0.1, 0.01)
#' cpop1_result = cpop1_iterate(z1, z2, y1, y2, w, nIter = 20,
#' alpha = alpha, n_features = 30, s = "lambda.min")
#' cpop2_result = cpop2_sign(z1, z2, y1, y2,
#' cpop1_result = cpop1_result, s = "lambda.min", nIter = 20, family = "binomial")
#' cpop3_result = cpop3(z1, z2, y1, y2,
#' cpop2_result = cpop2_result, family = "binomial", intercept = FALSE)
#' plot(predict(cpop3_result$en1, newx = z1[,cpop2_result], s = "lambda.min"),
#' predict(cpop3_result$en2, newx = z1[,cpop2_result], s = "lambda.min"))
#' abline(a = 0, b = 1)
#'
cpop3 = function(z1, z2, y1, y2, cpop2_result, intercept = FALSE, family = "binomial", ...){
    en1 = glmnet::cv.glmnet(
      x = z1[,cpop2_result],
      y = y1,
      family = family,
      alpha = 0,
      intercept = intercept,
      ...)

    en2 = glmnet::cv.glmnet(
      x = z2[,cpop2_result],
      y = y2,
      family = family,
      alpha = 0,
      intercept = intercept,
      ...)

  result = list(en1 = en1, en2 = en2, feature = cpop2_result)
  return(result)
}
