#' @title Step 1 of the TOP method
#' @description Step 1 of the TOP method, for a single given alpha
#' @param z1 A data matrix
#' @param z2 A data matrix
#' @param y1 A vector
#' @param y2 A vector
#' @param w A vector
#' @param nIter Number of iterations
#' @param alpha Lasso alpha
#' @param s CV-Lasso lambda
#' @param n_features n_features desired
#' @param ... Extra parameter settings for cv.glmnet
#' @param family See glmnet family
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
#' alpha = 0.1
#' s = "lambda.min"
#' top1(z1, z2, y1, y2, w, nIter = 20, n_features = 20, alpha = alpha, s = "lambda.min")
top1 = function(z1, z2, y1, y2, w, nIter = 20, alpha = 1, family = "binomial", n_features = 50, s = "lambda.min", ...){
  p = ncol(z1)
  remaining_features = colnames(z1)
  selected_features = c()

  for(i in 1:nIter){
    message("TOP1 - Step: ", sprintf("%02d", i), ": Number of selected features: ", length(selected_features), " out of ", p)

    if(length(selected_features) >= n_features) {
      message(n_features, " features was reached. ")
      message("A total of ", length(selected_features), " features were selected. \n")
      break
    }


    if(length(selected_features) == ncol(z1)) {
      message("All features are selected")
      break
    }



    en1 = glmnet::cv.glmnet(
      x = z1[,remaining_features],
      y = y1,
      family = family,
      penalty.factor = w[remaining_features],
      alpha = alpha,
      ...)

    en2 = glmnet::cv.glmnet(
      x = z2[,remaining_features],
      y = y2,
      family = family,
      penalty.factor = w[remaining_features],
      alpha = alpha,
      ...)

    selected_features = c(selected_features,
                          base::intersect(
                            rownames(get_lasso_coef(en1, s = s)),
                            rownames(get_lasso_coef(en2, s = s)))) %>% unique
    selected_features = selected_features[selected_features != "(Intercept)"]

    remaining_features = setdiff(colnames(z1), selected_features)
  } ## End i-loop

  return(selected_features)
}
###############
#' @title Step 1 of the TOP method, iteratred
#' @description Step 1 of the TOP method, for multiple alpha inputs
#' @param z1 A data matrix
#' @param z2 A data matrix
#' @param y1 A vector
#' @param y2 A vector
#' @param w A vector
#' @param nIter Number of iterations
#' @param alpha Lasso alpha, vector
#' @param s CV-Lasso lambda
#' @param n_features n_features desired
#' @param family see glmnet family
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
#' x2 = x1 + matrix(rnorm(n * p, mean = 0, sd = 0.1), nrow = n, ncol = p)
#' colnames(x1) = colnames(x2) = paste0("X", 1:p)
#' k = 2
#' beta = c(rep(1, k), rep(0, p - k))
#' expit = function(x) 1/(1+exp(-x))
#' y1 = rbinom(n, 1, prob = expit(x1 %*% beta))
#' y2 = rbinom(n, 1, prob = expit(x2 %*% beta))
#' z1 = pairwise_col_diff(x1)
#' z2 = pairwise_col_diff(x2)
#' w = compute_weights(z1, z2)
#' alpha = c(1, 0.1, 0.01)
#' top1_result = top1_iterate(z1, z2, y1, y2, w, nIter = 20,
#' alpha = alpha, n_features = 30, s = "lambda.min")
#' top1_result
top1_iterate = function(z1, z2, y1, y2, w,
                        nIter = 20, alpha = 1,
                        n_features = 50, family = "binomial",
                        s = "lambda.min", ...){

  # remaining_features = colnames(z1)
  all_selected_features = c()

  for(this_alpha in alpha){
    message("Fitting TOP model using alpha = ", this_alpha, "\n")
    updated_w = w
    updated_w[all_selected_features] = 0
    print(table(sign(updated_w)))

    this_top1_features = top1(z1, z2,
                              y1, y2,
                              w = updated_w, nIter = 20,
                              n_features = n_features, alpha = this_alpha, family = family,
                              s = "lambda.min")

    all_selected_features = unique(c(all_selected_features, this_top1_features))

    if(length(all_selected_features) >= n_features) {
      message(n_features, " features was reached. ")
      message("A total of ", length(all_selected_features), " features were selected. \n")
      break
    } else{

      # remaining_features = setdiff(colnames(z1), all_selected_features)
      message(n_features, " features was not reached. \n")
    }
  }

  return(all_selected_features)
}
