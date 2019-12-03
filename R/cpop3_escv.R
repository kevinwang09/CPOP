#' @title Step 3 of the CPOP method, HDCI
#' @description Step 3 of the CPOP method, HDCI
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
#' @return A vector
#' @export
#' @examples
#' data(cpop_data_binary, package = 'CPOP')
#' set.seed(1)
#' z1 = pairwise_col_diff(x1)
#' z2 = pairwise_col_diff(x2)
#' w = compute_weights(z1, z2)
#' cpop1_result = cpop1_iterate(
#' z1 = z1, z2 = z2, y1 = y1, y2 = y2, w = w,
#' family = "binomial", alpha = 0.1)
#' cpop2_hdci_result = cpop2_hdci(z1 = z1, z2 = z2, y1 = y1, y2 = y2,
#' cpop1_result = cpop1_result, family = "binomial")
#' cpop3_hdci_result = cpop3_hdci(z1, z2, y1, y2,
#' cpop2_result = cpop2_hdci_result, family = "binomial", intercept = FALSE)
cpop3_hdci = function(z1, z2, y1, y2, cpop2_result, family, intercept = FALSE, ...){
  glmnet1 = HDCI::escv.glmnet(
    x = z1[,cpop2_result],
    y = y1,
    family = family,
    alpha = 0,
    intercept = intercept,
    ...)

  glmnet2 = HDCI::escv.glmnet(
    x = z2[,cpop2_result],
    y = y2,
    family = family,
    alpha = 0,
    intercept = intercept,
    ...)

  result = list(glmnet1 = glmnet1,
                glmnet2 = glmnet2, feature = cpop2_result)
  return(result)
}
