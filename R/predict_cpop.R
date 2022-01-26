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
#' ## Loading simulated matrices and vectors
#' x1 = cpop_data_binary$x1
#' x2 = cpop_data_binary$x2
#' x3 = cpop_data_binary$x3
#' y1 = cpop_data_binary$y1
#' y2 = cpop_data_binary$y2
#' y3 = cpop_data_binary$y3
#' set.seed(1)
#' z1 = pairwise_col_diff(x1)
#' z2 = pairwise_col_diff(x2)
#' cpop_result = cpop_model(z1, z2, y1, y2, alpha = 0.1, n_features = 10)
#' cpop_result
#' z3 = pairwise_col_diff(x3)
#' head(predict_cpop(cpop_result, newz = z3))
predict_cpop = function(cpop_result, newz, s = "lambda.min"){
  ## If any discovered featureset is not in newz, then stop
  assertthat::assert_that(all(cpop_result$feature %in% colnames(newz)),
                          msg = "cpop_result feature must be a strict subset of colnames(newz)")

  assertthat::assert_that(sum(is.na(newz)) == 0,
                          msg = "All entries of the prediction data must be non-missing. \n
                          You should try running an imputation method prior to calculating log-ratio matrices.")

  if(is.null(cpop_result)) return(NULL)

  result1_link = predict(object = cpop_result$glmnet1, newx = newz[,cpop_result$feature, drop = FALSE], s = s,
                    type = "link")
  result2_link = predict(object = cpop_result$glmnet2, newx = newz[,cpop_result$feature, drop = FALSE], s = s,
                    type = "link")

  result_mat = cbind(result1_link, result2_link, (result1_link + result2_link)/2)
  colnames(result_mat) = c("cpop_model1", "cpop_model2", "cpop_model_avg")

  if(is.null(rownames(result_mat))){
    rownames(result_mat) = 1:nrow(result_mat)
  }

  tib_result = tibble::as_tibble(data.frame(result_mat))
  tib_result = dplyr::mutate(tib_result, samples = rownames(result_mat))

  if(cpop_result$family_params$family == "binomial"){
    result1_prob = predict(object = cpop_result$glmnet1, newx = newz[,cpop_result$feature, drop = FALSE], s = s,
                               type = "response")
    result2_prob = predict(object = cpop_result$glmnet2, newx = newz[,cpop_result$feature, drop = FALSE], s = s,
                               type = "response")
    cpop_model_avg_prob = (as.vector(result1_prob) + as.vector(result2_prob))/2
    cpop_model_avg_class = dplyr::case_when(
      cpop_model_avg_prob < 0.5 ~ cpop_result$family_params$factor_levels[1],
      cpop_model_avg_prob > 0.5 ~ cpop_result$family_params$factor_levels[2],
      TRUE ~ NA_character_)
    tib_result = dplyr::mutate(tib_result,
                       cpop_model_avg_prob = cpop_model_avg_prob,
                       cpop_model_avg_class = cpop_model_avg_class)
  }
  tib_result = dplyr::select(tib_result, samples, dplyr::everything())
  return(tib_result)
}

#' @title Imputing gene expression values using CPOP model
#' @description Imputing gene expression values using CPOP model
#' @param cpop_result cpop_model result
#' @param x1 gene data 1
#' @param x2 gene data 2
#' @param newx gene gene data
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
#' z1 = pairwise_col_diff(x1)
#' z2 = pairwise_col_diff(x2)
#' cpop_result = cpop_model(z1, z2, y1, y2, alpha = 0.1, n_features = 10)
#' cpop_result
#' z3 = pairwise_col_diff(x3)
#' head(predict_cpop(cpop_result, newz = z3))
#' x4 = x3
#' x4[,1] = NA
#' # z4 = pairwise_col_diff(x4)
#' ## head(predict_cpop(cpop_result, newz = z4))
#' x4_imp = impute_cpop(cpop_result, x1 = x1, x2 = x2, newx = x4)
#' # head(predict_cpop(cpop_result, newz = CPOP::pairwise_col_diff(x4_imp)))
#' ## plot(predict_cpop(cpop_result, newz = CPOP::pairwise_col_diff(x4_imp))$cpop_model_avg,
#' ## predict_cpop(cpop_result, newz = z3)$cpop_model_avg)
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
    x1_pred = predict(g_x1, newx = cbind(miss_data_intersect_x1x2[,!which_col_missing, drop = FALSE]))
    x2_pred = predict(g_x2, newx = cbind(miss_data_intersect_x1x2[,!which_col_missing, drop = FALSE]))
    miss_data_intersect_x1x2[,this_miss_col] = as.numeric(x1_pred + x2_pred)/2
  }

  return(miss_data_intersect_x1x2)
}
