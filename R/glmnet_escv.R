#' @title Step 1 of the CPOP method
#' @description Step 1 of the CPOP method, for a single given alpha
#' @param x A data matrix
#' @param y A vector
#' @param n_folds Number of CV folds
#' @param lambda Default to 10^seq(2, -3, length = 100)
#' @param ... extra parameters pass onto glmnet
#' @importFrom glmnet glmnet
#' @importFrom glmnet coef.glmnet
#' @importFrom caret createFolds
#' @importFrom e1071 skewness
#' @return A vector
#' @export
#' @examples
#' set.seed(1)
#' n = 1000
#' p = 10
#' x1 = matrix(rnorm(n * p, mean = 0, sd = 1), nrow = n, ncol = p)
#' x2 = x1 + matrix(rnorm(n * p, mean = 0, sd = 0.1), nrow = n, ncol = p)
#' colnames(x1) = colnames(x2) = sprintf("X%02d", 1:p)
#' k = 2
#' beta = c(rep(1, k), rep(0, p - k))
#' y1 = x1 %*% beta + rnorm(n, mean = 0, sd = 0.5)
#' y2 = x2 %*% beta + rnorm(n, mean = 0, sd = 0.5)
#' glmnet_escv(x = x1, y = y1)

glmnet_escv = function(x, y, n_folds = 5, lambda = 10^seq(3, -3, length = 100), ...){
  test_index = caret::createFolds(y = y, k = n_folds)
  list_test_x = purrr::map(.x = test_index, .f = ~x[.x, ])
  list_test_y = purrr::map(.x = test_index, .f = ~y[.x])
  list_train_x = purrr::map(.x = test_index, .f = ~x[-.x, ])
  list_train_y = purrr::map(.x = test_index, .f = ~y[-.x])

  full_glmnet = glmnet::glmnet(x = x, y = y, lambda = lambda)

  list_glmnet = purrr::map2(
    .x = list_train_x,
    .y = list_train_y,
    .f = ~ glmnet::glmnet(x = .x, y = .y, lambda = lambda))

  yhat_fold_lambda = purrr::map(
    .x = list_glmnet,
    .f = ~ glmnet::predict.glmnet(object = .x, newx = x, s = lambda))

  yhat_bar = Reduce("+", yhat_fold_lambda)/n_folds

  es_numerator = purrr::map(
    .x = yhat_fold_lambda,
    .f = ~ col2norm(.x - yhat_bar)^2) %>%
    Reduce("+", .)/n_folds

  es_lambda = es_numerator/col2norm(yhat_bar)^2

  es_lambda_min = lambda[which.min(es_lambda)]



  plot(y, predict(full_glmnet, newx = x, s = es_lambda_min))
  abline(a = 0, b = 1, col = "red")
  print(new_iden_dist(y, predict(full_glmnet, newx = x, s = es_lambda_min)))

  # plot(y1, predict(full_glmnet, newx = x, s = es_lambda_min) + rexp(n))
  # abline(a = 0, b = 1, col = "red")
  # plot(es_lambda, type = "l", log = "y")
  # points(which.min(es_lambda), es_lambda[which.min(es_lambda)], pch = 18)

}


new_iden_dist = function(x, y){
  CPOP::identityDist(x, y) +
    abs(e1071::skewness(x-y))
}

subtract_yhat_bar = function(yhat_one_fold){
  purrr::map(
    .x = seq_along(lambda),
    .f = ~ yhat_one_fold[,.x] - yhat_bar[,.x])
}
col2norm = function(X){apply(X = X, MARGIN = 2, FUN = base::norm, type = "2")}
col1norm = function(X){Matrix::colSums(abs(X))}
