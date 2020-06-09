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
#' attach(cpop_data_binary)
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
#' @return A vector
#' @export
#' @examples
#' data(cpop_data_binary, package = 'CPOP')
#' attach(cpop_data_binary)
#' set.seed(1)
#' z1 = pairwise_col_diff(x1)
#' z2 = pairwise_col_diff(x2)
#' cpop_result = cpop_model(z1, z2, y1, y2, alpha = 0.1, n_features = 10)
#' cpop_result
#' z3 = pairwise_col_diff(x3)
#' head(predict_cpop(cpop_result, newz = z3))
#' x4 = x3
#' x4[,1] = NA
#' z4 = pairwise_col_diff(x4)
#' ## head(predict_cpop(cpop_result, newz = z4))
#' x4_imp = impute_cpop(cpop_result, x1 = x1, x2 = x2, newx = x4)
#' head(predict_cpop(cpop_result, newz = CPOP::pairwise_col_diff(x4_imp)))
#' ## plot(predict_cpop(cpop_result, newz = CPOP::pairwise_col_diff(x4_imp))$cpop_model_avg,
#' ## predict_cpop(cpop_result, newz = z3)$cpop_model_avg)
impute_cpop = function(cpop_result, x1, x2, newx){
  cpop_lr2genes = stringr::str_split(cpop_result$feature, "--") %>% unlist %>% unique %>% sort
  miss_data = newx
  which_col_missing = colSums(is.na(miss_data)) > 0 ## Which column has any missing values
  miss_names = colnames(newx)[which_col_missing]

  for(this_miss_col in miss_names){
    x1_without_miss = x1[,!which_col_missing] ## Complete data
    x2_without_miss = x2[,!which_col_missing]

    g_x1 = glmnet::cv.glmnet(x = x1_without_miss,
                             y = x1[,this_miss_col],
                             family = "gaussian", nfolds = 5)

    g_x2 = glmnet::cv.glmnet(x = x2_without_miss,
                             y = x2[,this_miss_col],
                             family = "gaussian", nfolds = 5)
    x1_pred = predict(g_x1, newx = cbind(miss_data[,!which_col_missing]))
    x2_pred = predict(g_x2, newx = cbind(miss_data[,!which_col_missing]))
    miss_data[,this_miss_col] = (x1_pred + x2_pred)/2
  }

  return(miss_data)
}
