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
#' @importFrom glmnet coef.glmnet
#' @rdname cpop3
#' @return A vector
#' @export
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
