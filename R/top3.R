#' @title Step 3 of the TOP method iterate
#' @description Step 3 of the TOP method
#' @param z1 A data matrix
#' @param z2 A data matrix
#' @param y1 A vector
#' @param y2 A vector
#' @param top2_result top2 result
#' @param intercept default to FALSE
#' @param ... Extra parameter settings for cv.glmnet
#' @importFrom glmnet cv.glmnet
#' @importFrom glmnet coef.cv.glmnet
#' @return A vector
#' @export
#' @examples
#' set.seed(1)
#' n = 1000
#' p = 10
#' x1 = matrix(rnorm(n * p, mean = 0, sd = 1), nrow = n, ncol = p)
#' x2 = x1 + 0.1
#' colnames(x1) = colnames(x2) = paste0("X", 1:p)
#' k = 2
#' beta = c(rep(1, k), rep(0, p - k))
#' expit = function(x) 1/(1+exp(-x))
#' y1 = rbinom(n, 1, prob = expit(x1 %*% beta))
#' y2 = rbinom(n, 1, prob = expit(x2 %*% beta))
#' z1 = pairwise_col_diff(x1)
#' z2 = pairwise_col_diff(x2)
#' w = compute_weights(z1, z2)
#' nIter = 20
#' top1_result = top1(z1, z2, y1, y2, w, nIter = 20, alpha = 1, s = "lambda.min")
#' s = "lambda.min"
#' top2_result = top2(z1, z2, y1, y2,
#' top1_result = top1_result, s = "lambda.min", nIter = 20, intercept = FALSE)
#' top3_result = top3(z1, z2, y1, y2, top2_result = top2_result, intercept = FALSE)
#' plot(predict(top3_result$en1, newx = z1[,top2_result], s = "lambda.min"),
#' predict(top3_result$en2, newx = z1[,top2_result], s = "lambda.min"))
#' abline(a = 0, b = 1)
#'
top3 = function(z1, z2, y1, y2, top2_result, intercept = FALSE, ...){
    en1 = glmnet::cv.glmnet(
      x = z1[,top2_result],
      y = y1,
      family = "binomial",
      alpha = 0,
      intercept = intercept,
      ...)

    en2 = glmnet::cv.glmnet(
      x = z2[,top2_result],
      y = y2,
      family = "binomial",
      alpha = 0,
      intercept = intercept,
      ...)

  result = list(en1 = en1, en2 = en2, feature = top2_result)
  return(result)
}
