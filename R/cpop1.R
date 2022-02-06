#' @title CPOP internal functions
#' @description Step 1 of the CPOP method, aiming to select features agreed by
#' both input data.
#' @param z1 A data matrix, columns are pairwise-differences between
#' the original data columns.
#' @param z2 A data matrix, columns are pairwise-differences between
#' the original data columns.
#' Column names should be identical to z1.
#' @param y1 A vector of response variable.
#' Must be of the same length as the number of rows of z1.
#' @param y2 A vector of response variable.
#' Must be of the same length as the number of rows of z2.
#' @param w A vector of weights to encourage selection of features agreed by both data.
#' Default to NULL, in which case, the absolute difference between column-wise means
#' are used.
#' @param n_iter Number of iterations for `cpop1` and `cpop2` functions.
#' @param alpha The alpha parameter for elastic net models.
#' See the `alpha` argument in glmnet::glmnet.
#' @param n_features Breaking the CPOP-Step 1 loop if a certain number of features is reached.
#' @param s Method to select a lambda estimate, either "lambda.min" (default) or "lambda.1se".
#' @param cpop1_method
#' \itemize{
#' \item "normal" (default): meaning that the features selected by **both** data1 (consisted of z1 and y1) and
#' data2 (consisted of z2 and y2) will be used to construct the final feature set in the first step of CPOP.
#' \item "after": In case that no predictive features were found to be commonly predictive in both data,
#' features ever found by **both** data will be pooled to construct the final feature set in the first step of CPOP.
#' \item "either": In case that no predictive features were found to be commonly predictive in both data,
#' features ever selected by **either** data will now be pooled
#' }
#' @param ... Extra parameter settings for glmnet::cv.glmnet
#' @param family See family in glmnet::glmnet function.
#' @importFrom glmnet cv.glmnet
#' @importFrom glmnet coef.glmnet
#' @importFrom tibble lst
#' @importFrom dplyr bind_rows mutate %>%
#' @importFrom rlang .data
#' @rdname cpop_internals
#' @return A list. Consisted of a vector of features and a tibble of features selected in each step.
#' @export
cpop1 = function(z1, z2, y1, y2, w, family, n_iter = 20, alpha = 1,
                 n_features = 50, s = "lambda.min", cpop1_method = "normal", ...){
  ## Initialising the selected feature set
  p = ncol(z1)
  remaining_features = colnames(z1)
  step_features = vector("list", length = n_iter)
  selected_features = do.call(c, step_features)

  ## For each iteration
  for(i in 1:n_iter){
    ## Print out the number of selected features in each iteration
    message("CPOP1 - Step ", sprintf("%02d", i),
            ": Number of selected features: ", length(selected_features),
            " out of ", p)
    ## If we exceed the number of desired features, we will stop the iterations
    if(length(selected_features) >= n_features) {
      message(n_features, " features was reached. ")
      message("A total of ", length(selected_features), " features were selected. \n")
      break
    }

    ## If we exhaust all features, we will stop the iterations
    if(length(selected_features) == p) {
      message("All features are selected, break now")
      break
    }


    ## Elastic net model 1 is for the first data.
    ## We will fit the data with only the remaining features with weights
    en1 = glmnet::cv.glmnet(
      x = z1[,remaining_features, drop = FALSE],
      y = y1,
      family = family,
      penalty.factor = w[remaining_features],
      alpha = alpha,
      ...)

    ## Elastic net model 2 is for the second data.
    en2 = glmnet::cv.glmnet(
      x = z2[,remaining_features, drop = FALSE],
      y = y2,
      family = family,
      penalty.factor = w[remaining_features],
      alpha = alpha,
      ...)

    ## The selected feature set is a concatenation of the
    ## existing selected features with the common features jointly selected by
    ## The two elastic net models.
    en1_coef = get_lasso_coef(en1, s = s)
    en2_coef = get_lasso_coef(en2, s = s)

    en1_coef_tbl = feature_tibble(en1_coef, coef_model = "1")
    en2_coef_tbl = feature_tibble(en1_coef, coef_model = "2")

    ## This collects all the selected features by EN1 and EN2
    step_features[[i]] = dplyr::bind_rows(en1_coef_tbl, en2_coef_tbl)

    ## Features selected in this step of CPOP
    selected_features = c(selected_features,
                          base::intersect(
                            rownames(en1_coef),
                            rownames(en2_coef))) %>% unique

    selected_features = selected_features[selected_features != "(Intercept)"]

    ## The remaining features are the features not in the selected feature set
    remaining_features = setdiff(colnames(z1), selected_features)
  } ## End i-loop

  ## Finalise the feature set
  step_features_tbl =  dplyr::bind_rows(step_features, .id = "step")

  ## If there are no features selected, then
  if(length(selected_features) == 0 & cpop1_method == "after"){
    warning("No predictive features commonly predictive in both data (at each iteration) were found \n alternative feature set was be used")
    message("Features ever selected by both data (after all iterations) will now be pooled")
    selected_features = step_features_tbl %>%
      dplyr::filter(.data$feature_name != "(Intercept)") %>%
      dplyr::select(.data$coef_model, .data$feature_name) %>%
      dplyr::distinct(.data$coef_model, .data$feature_name) %>%
      dplyr::group_by(.data$feature_name) %>%
      dplyr::tally() %>%
      dplyr::filter(n == 2) %>%
      dplyr::pull(.data$feature_name)
  } else if(length(selected_features) == 0 & cpop1_method == "either") {
    warning("No predictive features commonly predictive in both data (at each iteration) were found \n alternative feature set was be used")
    message("Features ever selected by either data will now be pooled")
    selected_features = step_features_tbl %>%
      dplyr::filter(.data$feature_name != "(Intercept)") %>%
      dplyr::pull(.data$feature_name) %>% unique()
  }

  if(length(selected_features) == 0){ ## If there are no features selected, then we return NULL
    return(NULL)
  } else { ## If there are selected features, then we perform further processing
    final_features = mst_lratio(selected_features)
    message("Removing sources of collinearity gives ", length(final_features), " features. \n")

    step_features_tbl = step_features_tbl %>%
      dplyr::mutate(
        ever_selected_features = .data$feature_name %in% selected_features,
        in_final_features = .data$feature_name %in% final_features)

    return(tibble::lst(final_features, step_features_tbl))
  }
}
###########################################################################
feature_tibble = function(en_coef, coef_model = NA){
  if(nrow(en_coef) == 0){
    en_coef_tbl = tibble::tibble(
      coef_model = coef_model,
      feature_name = NA,
      feature_value = NA)
  } else {
    en_coef_tbl = tibble::tibble(
      coef_model = coef_model,
      feature_name = rownames(en_coef),
      feature_value = as.vector(as.matrix(en_coef)))
  }
  return(en_coef_tbl)
}
###########################################################################
#' @title Step 1 of the CPOP method, iteratred over multiple alpha
#' @description Step 1 of the CPOP method, for multiple alpha inputs
#' @importFrom glmnet cv.glmnet
#' @importFrom glmnet coef.glmnet
#' @rdname cpop_internals
#' @return A vector of features
#' @export
cpop1_iterate = function(z1, z2, y1, y2, w = NULL,
                         family, s = "lambda.min",
                         n_iter = 20, alpha = 1,
                         n_features = 50, ...){

  alpha = sort(alpha, decreasing = TRUE)
  all_selected_features = c()

  if(is.null(w)){
    w = colmeans_penalty(z1, z2)
    message("Absolute colMeans difference will be used as the weights for CPOP")
  }

  for(this_alpha in alpha){
    message("Fitting CPOP model using alpha = ", this_alpha, "\n")
    updated_w = w
    updated_w[all_selected_features] = 0
    message("Based on previous alpha, ", sum(updated_w == 0), " features are kept \n")

    cpop1_result = cpop1(
      z1 = z1, z2 = z2, y1 = y1, y2 = y2,
      w = updated_w, n_iter = n_iter,
      n_features = n_features, alpha = this_alpha, family = family, s = s, ...)

    all_selected_features = unique(c(all_selected_features, cpop1_result$final_features))

    if(length(all_selected_features) >= n_features) {
      message(n_features, " features was reached. ")
      message("A total of ", length(all_selected_features), " features were selected. \n")
      break
    } else{

      # remaining_features = setdiff(colnames(z1), all_selected_features)
      message(n_features, " features was not reached. \n")
    }
  }

  if(length(all_selected_features) == 0){
    return(NULL)
  } else {
    return(tibble::lst(cpop1_features = all_selected_features,
                       step_features = cpop1_result$step_features))
  }
}
