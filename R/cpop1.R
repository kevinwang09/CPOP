#' @title Step 1 of the CPOP method
#' @description Step 1 of the CPOP method, for a single given alpha.
#' Step 1 of CPOP takes in two training data (x1, y1) and (x2, y2) and performed weighted
#' elastic net variable selection.
#' @param z1 A data matrix
#' @param z2 A data matrix
#' @param y1 A vector
#' @param y2 A vector
#' @param w A vector
#' @param n_iter Number of iterations
#' @param alpha Lasso alpha
#' @param n_features n_features desired
#' @param s CV lambda
#' @param cpop1_method Default value is "normal". Alternatives are "after" and "either".
#' @param ... Extra parameter settings for cv.glmnet
#' @param family See glmnet family
#' @importFrom glmnet cv.glmnet
#' @importFrom glmnet coef.glmnet
#' @importFrom tibble lst
#' @importFrom dplyr bind_rows mutate %>%
#' @rdname cpop1
#' @return A vector of features
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
      x = z1[,remaining_features],
      y = y1,
      family = family,
      penalty.factor = w[remaining_features],
      alpha = alpha,
      ...)

    ## Elastic net model 2 is for the second data.
    en2 = glmnet::cv.glmnet(
      x = z2[,remaining_features],
      y = y2,
      family = family,
      penalty.factor = w[remaining_features],
      alpha = alpha,
      ...)

    ## The selected feature set is a concatenation of the
    ## existing selected features withthe common features jointly selected by
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
      dplyr::filter(feature_name != "(Intercept)") %>%
      dplyr::select(coef_model, feature_name) %>%
      dplyr::distinct(coef_model, feature_name) %>%
      dplyr::group_by(feature_name) %>%
      dplyr::tally() %>%
      dplyr::filter(n == 2) %>%
      dplyr::pull(feature_name)
  } else if(length(selected_features) == 0 & cpop1_method == "either") {
    warning("No predictive features commonly predictive in both data (at each iteration) were found \n alternative feature set was be used")
    message("Features ever selected by either data will now be pooled")
    selected_features = step_features_tbl %>%
      dplyr::filter(feature_name != "(Intercept)") %>%
      dplyr::pull(feature_name) %>% unique()
  }

  final_features = mst_lratio(selected_features)
  message("Removing sources of collinearity gives ", length(final_features), " features. \n")

  step_features_tbl = step_features_tbl %>%
    dplyr::mutate(
      ever_selected_features = feature_name %in% selected_features,
      in_final_features = feature_name %in% final_features)


  return(tibble::lst(final_features, step_features_tbl))
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
#' @rdname cpop1
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

  return(tibble::lst(cpop1_features = all_selected_features,
                     step_features = cpop1_result$step_features))
}
