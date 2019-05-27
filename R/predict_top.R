#' @title Prediction function for TOP
#' @description Prediction function for TOP
#' @param top_result top_model result
#' @param newz New data
#' @param s CV-Lasso lambda
#' @param model_number 1 or 2 or "both"
#' @param tibble Logical
#' @importFrom glmnet cv.glmnet
#' @importFrom glmnet coef.cv.glmnet
#' @importFrom glmnet predict.cv.glmnet
#' @importFrom tibble as_tibble
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
#' top_result = top_model(z1, z2, y1, y2, w, nIter = 20, alpha = 1, s = "lambda.min")
#' head(predict_top(top_result, newz = z1, model_number = 1))
#' head(predict_top(top_result, newz = z1, model_number = "both", tibble = TRUE))
predict_top = function(top_result, newz, s = "lambda.min", model_number = 1L, tibble = FALSE){
  ## If any discovered featureset is not in newz, then stop
  if(!all(top_result$feature %in% colnames(newz))){
    stop("top_result$feature must be a strict subset of colnames(newz)")
  }

  if(model_number == 1L){
    result = glmnet::predict.cv.glmnet(object = top_result$en1, newx = newz[,top_result$feature], s = s)
    colnames(result) = c("top_model1")
  }

  if(model_number == 2){
    result = glmnet::predict.cv.glmnet(object = top_result$en2, newx = newz[,top_result$feature], s = s)
    colnames(result) = c("top_model2")
  }

  if(model_number == "both"){
    result1 = glmnet::predict.cv.glmnet(object = top_result$en1, newx = newz[,top_result$feature], s = s)
    result2 = glmnet::predict.cv.glmnet(object = top_result$en2, newx = newz[,top_result$feature], s = s)
    result = cbind(result1, result2)
    colnames(result) = c("top_model1", "top_model2")
  }

  # rownames(result) = rownames(newz)

  if(tibble){
    result = tibble::as_tibble(data.frame(result))
    result = tibble::rownames_to_column(result, var = "samples")
  }

  return(result)
}
