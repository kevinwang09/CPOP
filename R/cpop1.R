#' @title Step 1 of the CPOP method
#' @description Step 1 of the CPOP method, for a single given alpha
#' @param z1 A data matrix
#' @param z2 A data matrix
#' @param y1 A vector
#' @param y2 A vector
#' @param w A vector
#' @param n_iter Number of iterations
#' @param alpha Lasso alpha
#' @param n_features n_features desired
#' @param s CV lambda
#' @param ... Extra parameter settings for cv.glmnet
#' @param family See glmnet family
#' @importFrom glmnet cv.glmnet
#' @importFrom glmnet coef.glmnet
#' @importFrom tibble lst
#' @importFrom dplyr bind_rows mutate %>%
#' @rdname cpop1
#' @return A vector of features
#' @export
#' @examples
#' data(cpop_data_binary, package = 'CPOP')
#' set.seed(1)
#' z1 = pairwise_col_diff(x1)
#' z2 = pairwise_col_diff(x2)
#' w = compute_weights(z1, z2)
#' cpop1(z1 = z1, z2 = z2, y1 = y1, y2 = y2, w = w, family = "binomial", alpha = 1)
cpop1 = function(z1, z2, y1, y2, w, family, n_iter = 20, alpha = 1, n_features = 50, s = "lambda.min", ...){
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

    if(nrow(en1_coef) == 0){
      en1_coef_tbl = tibble::tibble(
        coef_model = "1",
        feature_name = NA,
        feature_value = NA)
    } else {
      en1_coef_tbl = tibble::tibble(
        coef_model = "1",
        feature_name = rownames(en1_coef),
        feature_value = as.vector(as.matrix(en1_coef)))
    }

    if(nrow(en2_coef) == 0){
      en2_coef_tbl = tibble::tibble(
        coef_model = "2",
        feature_name = NA,
        feature_value = NA)
    } else {
      en2_coef_tbl = tibble::tibble(
        coef_model = "2",
        feature_name = rownames(en2_coef),
        feature_value = as.vector(as.matrix(en2_coef)))
    }


    step_features[[i]] = dplyr::bind_rows(en1_coef_tbl, en2_coef_tbl)

    selected_features = c(selected_features,
                          base::intersect(
                            rownames(en1_coef),
                            rownames(en2_coef))) %>% unique
    selected_features = selected_features[selected_features != "(Intercept)"]

    ## The remaining features are the features not in the selected feature set
    remaining_features = setdiff(colnames(z1), selected_features)
  } ## End i-loop



  if(length(selected_features) != 0){
  final_features = rownames(get_lasso_coef(glmnet::cv.glmnet(
    x = z2[,selected_features],
    y = y2,
    family = family,
    alpha = alpha, ...)))[-1]
  message("Removing sources of collinearity gives ", length(final_features), " features. \n")
  } else {
    final_features = NULL
  }



  step_features = dplyr::bind_rows(step_features, .id = "step") %>%
    dplyr::mutate(
      ever_selected_features = feature_name %in% selected_features,
      in_final_features = feature_name %in% final_features
    )

  return(tibble::lst(final_features,
             step_features))
}
###############
#' @title Step 1 of the CPOP method, iteratred over multiple alpha
#' @description Step 1 of the CPOP method, for multiple alpha inputs
#' @importFrom glmnet cv.glmnet
#' @importFrom glmnet coef.glmnet
#' @rdname cpop1
#' @return A vector of features
#' @export
#' @examples
#' data(cpop_data_binary, package = 'CPOP')
#' set.seed(1)
#' z1 = pairwise_col_diff(x1)
#' z2 = pairwise_col_diff(x2)
#' w = compute_weights(z1, z2)
#' cpop1_iterate(z1 = z1, z2 = z2, y1 = y1, y2 = y2, w = w, family = "binomial", alpha = c(0.5, 1))
cpop1_iterate = function(z1, z2, y1, y2, w,
                         family, s = "lambda.min",
                         n_iter = 20, alpha = 1,
                         n_features = 50, ...){

  alpha = sort(alpha, decreasing = TRUE)
  all_selected_features = c()

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
