#' @title Prediction function for CPOP
#' @description Prediction function for CPOP
#' @param cpop_result cpop_model result
#' @param newz New data
#' @param s CV-Lasso lambda
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
#' cpop_result = cpop_model(z1, z2, y1, y2, alpha = 0.1, n_features = 10)
#' cpop_result
#' newz = z1[sample(1:nrow(z1), 10),]
#' head(predict_cpop(cpop_result, newz = newz))
predict_cpop = function(cpop_result, newz, s = "lambda.min"){
  ## If any discovered featureset is not in newz, then stop
  assertthat::assert_that(all(cpop_result$feature %in% colnames(newz)),
                          msg = "cpop_result feature must be a strict subset of colnames(newz)")

    result1 = predict(object = cpop_result$glmnet1, newx = newz[,cpop_result$feature], s = s)
    result2 = predict(object = cpop_result$glmnet2, newx = newz[,cpop_result$feature], s = s)

    result_mat = cbind(result1, result2, (result1 + result2)/2)
    colnames(result_mat) = c("cpop_model1", "cpop_model2", "cpop_model_avg")
    if(is.null(rownames(result_mat))){
      rownames(result_mat) = 1:nrow(result_mat)
    }


    tib_result = tibble::as_tibble(data.frame(result_mat))
    tib_result = dplyr::mutate(tib_result, samples = rownames(result_mat))
    tib_result = dplyr::select(tib_result, samples, dplyr::everything())
    return(tib_result)
}
