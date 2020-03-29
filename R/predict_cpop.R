#' @title Prediction function for CPOP
#' @description Prediction function for CPOP
#' @param cpop_result cpop_model result
#' @param newz New data
#' @param s CV-Lasso lambda
#' @param model_number 1 or 2 or "both"
#' @param tibble Logical
#' @importFrom glmnet cv.glmnet
#' @importFrom glmnet coef.glmnet
#' @importFrom tibble as_tibble
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr everything
#' @return A vector
#' @export
#' @examples
#' data(cpop_data_binary, package = 'CPOP')
#' set.seed(1)
#' z1 = pairwise_col_diff(x1)
#' z2 = pairwise_col_diff(x2)
#' w = compute_weights(z1, z2)
#' cpop_result = cpop_model(z1, z2, y1, y2, w = w,
#' alpha = 1, n_features = 40, s = "lambda.min", family = "binomial")
#' rownames(z1) = paste0("sample",1:nrow(z1))
#' newz = z1[sample(1:5, 10, replace = TRUE),]
#' head(predict_cpop(cpop_result, newz = newz, model_number = 1))
#' head(predict_cpop(cpop_result, newz = newz, model_number = "both"))
#' head(predict_cpop(cpop_result, newz = newz, model_number = "avg"))
predict_cpop = function(cpop_result, newz, s = "lambda.min", model_number = 1L, tibble = TRUE){
  ## If any discovered featureset is not in newz, then stop
  if(!all(cpop_result$feature %in% colnames(newz))){
    stop("cpop_result feature must be a strict subset of colnames(newz)")
  }

  if(model_number == 1L){
    result = predict(object = cpop_result$glmnet1, newx = newz[,cpop_result$feature], s = s)
    colnames(result) = c("cpop_model1")
  }

  if(model_number == 2){
    result = predict(object = cpop_result$glmnet2, newx = newz[,cpop_result$feature], s = s)
    colnames(result) = c("cpop_model2")
  }

  if(model_number == "both"){
    result1 = predict(object = cpop_result$glmnet1, newx = newz[,cpop_result$feature], s = s)
    result2 = predict(object = cpop_result$glmnet2, newx = newz[,cpop_result$feature], s = s)
    result = cbind(result1, result2)
    colnames(result) = c("cpop_model1", "cpop_model2")
  }

  if(model_number == "avg"){
    result1 = predict(object = cpop_result$glmnet1, newx = newz[,cpop_result$feature], s = s)
    result2 = predict(object = cpop_result$glmnet2, newx = newz[,cpop_result$feature], s = s)

    result = cbind(result1, result2, (result1 + result2)/2)
    colnames(result) = c("cpop_model1", "cpop_model2", "cpop_model_avg")
  }



  if(tibble){
    tib_result = tibble::as_tibble(data.frame(result))
    if(is.null(rownames(result))){
      rownames(result) = 1:nrow(result)
    }
    tib_result = dplyr::mutate(tib_result, samples = rownames(result))
    tib_result = dplyr::select(tib_result, samples, dplyr::everything())
    return(tib_result)
  } else {
    rownames(result) = rownames(newz)
    return(result)
  }
}
