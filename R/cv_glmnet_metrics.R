# # data(cpop_data, package = 'CPOP')
# set.seed(1234)
# col2norm = function(X){apply(X = X, MARGIN = 2, FUN = base::norm, type = "2")}
# col1norm = function(X){Matrix::colSums(abs(X))}
# n = 1000
# p = 10
# x1 = matrix(rnorm(n * p, mean = 0, sd = 1), nrow = n, ncol = p)
# colnames(x1) = paste0("X", 1:p)
# k = 1
# beta = c(rep(0.5, k), rep(0, p - k))
# y1 = x1 %*% beta + rnorm(n, 0, 1)
#
#
# library(caret)
# n_folds = 5
# test_index = caret::createFolds(y = y1, k = n_folds)
# list_test_x = purrr::map(.x = test_index, .f = ~x1[.x, ])
# list_test_y = purrr::map(.x = test_index, .f = ~y1[.x])
# list_train_x = purrr::map(.x = test_index, .f = ~x1[-.x, ])
# list_train_y = purrr::map(.x = test_index, .f = ~y1[-.x])
# lambda = 10^seq(2, -3, length = 100)
#
# cv_glmnet = glmnet::cv.glmnet(x = x1, y = y1, alpha = 0,
#                               lambda = lambda, intercept = FALSE)
# full_glmnet = glmnet::glmnet(x = x1, y = y1, alpha = 0,
#                               lambda = lambda, intercept = FALSE)
#
# list_glmnet = purrr::map2(
#   .x = list_train_x,
#   .y = list_train_y,
#   .f = ~ glmnet::glmnet(x = .x, y = .y,
#                         alpha = 0, lambda = lambda, intercept = FALSE))
#
# yhat_fold_lambda = purrr::map(
#   .x = list_glmnet,
#   .f = ~ glmnet::predict.glmnet(object = .x, newx = x1, s = lambda))
#
# yhat_bar = Reduce("+", yhat_fold_lambda)/n_folds
# dim(yhat_bar)
#
# subtract_yhat_bar = function(yhat_one_fold){
#   purrr::map(
#     .x = seq_along(lambda),
#     .f = ~ yhat_one_fold[,.x] - yhat_bar[,.x])
# }
#
# es_numerator = purrr::map(
#   .x = yhat_fold_lambda,
#   .f = ~ col2norm(.x - yhat_bar)^2) %>%
#   Reduce("+", .)/n_folds
#
# es_lambda = es_numerator/col2norm(yhat_bar)^2
#
# plot(es_lambda, type = "l", log = "y")
# points(which.min(es_lambda), es_lambda[which.min(es_lambda)], pch = 18)
#
#
# # full_glmnet_beta = coef.glmnet(full_glmnet, s = lambda)
# # full_glmnet_beta_L1 = col1norm(full_glmnet_beta)
# #
# # list_fold_loss_cv = purrr::map2(
# #   .x = list_test_x,
# #   .y = list_test_y,
# #   .f = ~ col2norm(.y - cbind(1, .x) %*% full_glmnet_beta) + lambda * full_glmnet_beta_L1
# # )
# #
# # plot(list_fold_loss_cv[[1]])
#
# ytesthat_fold_lambda = purrr::map2(
#   .x = list_glmnet,
#   .y = list_test_x,
#   .f = ~ glmnet::predict.glmnet(object = .x, newx = .y, s = lambda))
#
# list_fold_loss_cv = purrr::map2(
#   .x = list_test_y,
#   .y = ytesthat_fold_lambda,
#   .f = ~ col2norm(.x - .y)^2)
#
#
# list_fold_beta = purrr::map(.x = list_glmnet,
#                             .f = glmnet::coef.glmnet)
#
# list_fold_beta_L1 = purrr::map(list_fold_beta, col1norm)
#
# list_loss_cv = purrr::map2(
#   .x  = list_fold_loss_cv,
#   .y = list_fold_beta_L1,
#   .f = ~ .x + lambda*.y)
#
# plot(list_loss_cv[[1]], type = "l")
# plot(list_loss_cv[[2]], type = "l")
# plot(list_loss_cv[[3]], type = "l")
# plot(list_loss_cv[[4]], type = "l")
# plot(list_loss_cv[[5]], type = "l")
#
# which(cv_glmnet$lambda == cv_glmnet$lambda.min)
# purrr::map_dbl(list_loss_cv, which.min) %>% mean
# which.min(es_lambda)
#
# cv_glmnet$lambda.min
# mean(lambda[purrr::map_dbl(list_loss_cv, which.min)])
# lambda[which.min(es_lambda)]
#
#
#
# plot(predict(full_glmnet, newx = x1, s = cv_glmnet$lambda.min),
#      y1)
#
# plot(predict(full_glmnet, newx = x1, s = mean(lambda[purrr::map_dbl(list_loss_cv, which.min)])),
#      y1)
#
# plot(predict(full_glmnet, newx = x1, s = lambda[which.min(es_lambda)]),
#      y1)
#
# plot(coef(full_glmnet, s = cv_glmnet$lambda.min),
#      coef(full_glmnet, s = mean(lambda[purrr::map_dbl(list_loss_cv, which.min)])))
# abline(a = 0, b = 1, col = "red")
#
#
# plot(coef(full_glmnet, s = cv_glmnet$lambda.min),
#      coef(full_glmnet, s = cv_glmnet$lambda.1se))
# abline(a = 0, b = 1, col = "red")
#
# plot(coef(full_glmnet, s = cv_glmnet$lambda.min),
#      coef(full_glmnet, s = lambda[which.min(es_lambda)]))
# abline(a = 0, b = 1, col = "red")
