#' @title Prediction function for CPOP
#' @description Prediction function for CPOP
#' @param cpop_result cpop_model result
#' @param newx New data, n times p, as original features,
#' @param newz (Deprecated) new data, n times choose(p, 2), as ratio features.
#' @param s CV-Lasso lambda
#' @importFrom glmnet cv.glmnet coef.glmnet predict.glmnet
#' @importFrom tibble as_tibble
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr everything
#' @return A vector
#' @export
#' @examples
#' data(cpop_data_binary, package = 'CPOP')
#' ## Loading simulated matrices and vectors
#' x1 = cpop_data_binary$x1
#' x2 = cpop_data_binary$x2
#' x3 = cpop_data_binary$x3
#' y1 = cpop_data_binary$y1
#' y2 = cpop_data_binary$y2
#' y3 = cpop_data_binary$y3
#' set.seed(1)
#' cpop_result = cpop_model(x1 = x1, x2 = x2, y1 = y1, y2 = y2, alpha = 0.1, n_features = 10)
#' cpop_result
#' head(predict_cpop(cpop_result, newx = x3))
predict_cpop = function(cpop_result, newx, s = "lambda.min", newz){
  if(missing(newx)){
    warning("The `newz` argument is now deprecated in preference for `newx`,
    CPOP prediction can still run.")
  }
  if(missing(newz)){
    newz = pairwise_col_diff(newx)
  }
  ## If any discovered feature-set is not in newz, then stop
  assertthat::assert_that(all(cpop_result$feature %in% colnames(newz)),
                          msg = "cpop_result feature must be a strict subset of colnames(newz)")

  assertthat::assert_that(
    sum(is.na(newz)) == 0,
    msg = "All entries of the prediction data must be non-missing.
    You should try running an imputation method on the input matrix.
    One option is to use `impute_cpop`")

  if(is.null(cpop_result)) return(NULL)

  if(cpop_result$cpop_mode == "glmnet"){
    result1_link = predict(object = cpop_result$model1, newx = newz[,cpop_result$feature, drop = FALSE], s = s,
                           type = "link")
    result2_link = predict(object = cpop_result$model2, newx = newz[,cpop_result$feature, drop = FALSE], s = s,
                           type = "link")
  } else {
    result1_link = stats::predict(object = cpop_result$model1, newx = newz[,cpop_result$feature, drop = FALSE], type = "link")
    result2_link = stats::predict(object = cpop_result$model2, newx = newz[,cpop_result$feature, drop = FALSE], type = "link")
  }

  result_mat = cbind(result1_link, result2_link, (result1_link + result2_link)/2)
  colnames(result_mat) = c("cpop_model1", "cpop_model2", "cpop_model_avg")

  if(is.null(rownames(result_mat))){
    rownames(result_mat) = 1:nrow(result_mat)
  }

  tib_result = tibble::as_tibble(data.frame(result_mat))
  tib_result = dplyr::mutate(tib_result, samples = rownames(result_mat))

  if(cpop_result$family_params$family == "binomial"){
    if(cpop_result$cpop_mode == "glmnet"){
      result1_prob = predict(object = cpop_result$model1, newx = newz[,cpop_result$feature, drop = FALSE], s = s,
                             type = "response")
      result2_prob = predict(object = cpop_result$model2, newx = newz[,cpop_result$feature, drop = FALSE], s = s,
                             type = "response")
    } else {
      result1_prob = predict(object = cpop_result$model1, newx = newz[,cpop_result$feature, drop = FALSE], type = "response")
      result2_prob = predict(object = cpop_result$model2, newx = newz[,cpop_result$feature, drop = FALSE], type = "response")
    }

    cpop_model_avg_prob = (as.vector(result1_prob) + as.vector(result2_prob))/2
    cpop_model_avg_class = dplyr::case_when(
      cpop_model_avg_prob <= 0.5 ~ cpop_result$family_params$factor_levels[1],
      cpop_model_avg_prob > 0.5 ~ cpop_result$family_params$factor_levels[2],
      TRUE ~ NA_character_)
    tib_result = dplyr::mutate(tib_result,
                       cpop_model_avg_prob = cpop_model_avg_prob,
                       cpop_model_avg_class = cpop_model_avg_class)
  }
  tib_result = dplyr::select(tib_result, .data$samples, dplyr::everything())
  return(tib_result)
}

#' @title Imputing gene expression values using CPOP model
#' @description Imputing gene expression values using CPOP model
#' @param cpop_result cpop_model result
#' @param x1 Original feature data matrix 1.
#' @param x2 Original feature data matrix 2.
#' @param newx New original feature data matrix, with missing values.
#' @importFrom glmnet cv.glmnet
#' @importFrom tibble as_tibble
#' @importFrom stringr str_split
#' @importFrom magrittr %>%
#' @importFrom assertthat assert_that
#' @return A vector
#' @export
#' @examples
#' data(cpop_data_binary, package = 'CPOP')
#' ## Loading simulated matrices and vectors
#' x1 = cpop_data_binary$x1
#' x2 = cpop_data_binary$x2
#' x3 = cpop_data_binary$x3
#' y1 = cpop_data_binary$y1
#' y2 = cpop_data_binary$y2
#' y3 = cpop_data_binary$y3
#' set.seed(1)
#' cpop_result = cpop_model(x1 = x1, x2 = x2, y1 = y1, y2 = y2, alpha = 0.1, n_features = 10)
#' cpop_result
#' x3_pred_result = predict_cpop(cpop_result, newx = x3)
#' head(x3_pred_result)
#' ## Introduce a column of missing values in a new matrix, x4.
#' x4 = x3
#' x4[,2] = NA
#' ## Without imputation, the prediction function would not work properly
#' ## This prompts the user to use an imputation on their data.
#' ## head(predict_cpop(cpop_result, newx = x4))
#' ## CPOP can perform imputation on the x4 matrix, before this matrix is converted into z4.
#' x4_imp = impute_cpop(cpop_result, x1 = x1, x2 = x2, newx = x4)
#' x4_pred_result = predict_cpop(cpop_result, newx = x4_imp)
#' head(x4_pred_result)
#' plot(x3_pred_result$cpop_model_avg_prob, x3_pred_result$cpop_model_avg_prob)
impute_cpop = function(cpop_result, x1, x2, newx){
  cpop_lr2genes = stringr::str_split(cpop_result$feature, "--") %>% unlist %>% unique %>% sort

  assertthat::assert_that(identical(colnames(x1), colnames(x2)),
                          msg = "The columns of x1 must be the same as x2")
  cols_intersect_x1x2_newx = intersect(colnames(x1), colnames(newx))

  x1_intersect_newx = x1[,cols_intersect_x1x2_newx, drop = FALSE]
  x2_intersect_newx = x2[,cols_intersect_x1x2_newx, drop = FALSE]

  miss_data = newx
  miss_data_intersect_x1x2 = miss_data[,cols_intersect_x1x2_newx, drop = FALSE]
  which_col_missing = colSums(is.na(miss_data_intersect_x1x2)) > 0 ## Which column has any missing values
  miss_names = colnames(miss_data_intersect_x1x2)[which_col_missing, drop = FALSE]

  for(this_miss_col in miss_names){
    x1_without_miss = x1_intersect_newx[,!which_col_missing, drop = FALSE] ## Complete data
    x2_without_miss = x2_intersect_newx[,!which_col_missing, drop = FALSE]

    g_x1 = glmnet::cv.glmnet(x = x1_without_miss,
                             y = x1[,this_miss_col, drop = FALSE],
                             family = "gaussian", nfolds = 5)

    g_x2 = glmnet::cv.glmnet(x = x2_without_miss,
                             y = x2[,this_miss_col, drop = FALSE],
                             family = "gaussian", nfolds = 5)
    x1_pred = stats::predict(g_x1, newx = cbind(miss_data_intersect_x1x2[,!which_col_missing, drop = FALSE]))
    x2_pred = stats::predict(g_x2, newx = cbind(miss_data_intersect_x1x2[,!which_col_missing, drop = FALSE]))
    miss_data_intersect_x1x2[,this_miss_col] = as.numeric(x1_pred + x2_pred)/2
  }

  return(miss_data_intersect_x1x2)
}
