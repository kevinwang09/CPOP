# set.seed(1234)
# n = 1000
# p = 10
# x1 = matrix(rnorm(n * n, mean = 0, sd = 1), nrow = n, ncol = p)
# x2 = matrix(rnorm(n * p, mean = 1, sd = 1), nrow = n, ncol = p)
# colnames(x1) = colnames(x2) = paste0("X", 1:p)
# k = 2
# beta = c(rep(1, k), rep(0, p - k))
# expit = function(x) 1/(1+exp(-x))
# y1 = rbinom(n, 1, prob = expit(x1 %*% beta))
# y2 = rbinom(n, 1, prob = expit(x2 %*% beta))
# z1 = pairwise_col_diff(x1)
# z2 = pairwise_col_diff(x2)
# w = compute_weights(z1, z2)
# nIter = 20
# top1_result = top1(z1, z2, y1, y2, w, nIter = 20, alpha = 1, s = "lambda.min")
# s = "lambda.min"
# # top2_result = top2_sign(z1, z2, y1, y2, top1_result = top1_result, s = "lambda.min", nIter = 20)
#
#
# p = length(top1_result)
# top2_features = top1_result
# lambda = 10^seq(0,-5,length=100)
#
# z1_reduced = z1[,top2_features]
# z2_reduced = z2[,top2_features]
#
# ridge1 = glmnet::cv.glmnet(
#   x = z1_reduced,
#   y = y1,
#   family = "binomial",
#   alpha = 0, lambda = lambda)
#
# ridge2 = glmnet::cv.glmnet(
#   x = z2_reduced,
#   y = y2,
#   family = "binomial",
#   alpha = 0, lambda = lambda)
#
# which(lambda == ridge1$lambda.min)
# which(lambda == ridge2$lambda.min)
#
# ridge1_coef = coef(ridge1, s = lambda)
# ridge2_coef = coef(ridge2, s = lambda)
# dim(ridge1_coef)
# dim(ridge2_coef)
#
# pred_z1_beta1 = cbind(1, z1_reduced) %*% ridge1_coef
# pred_z1_beta2 = cbind(1, z1_reduced) %*% ridge2_coef
# pred_z2_beta1 = cbind(1, z2_reduced) %*% ridge1_coef
# pred_z2_beta2 = cbind(1, z2_reduced) %*% ridge2_coef
#
#
# col2norm = function(X){apply(X = X, MARGIN = 2, FUN = base::norm, type = "2")}
# col1norm = function(X){Matrix::colSums(abs(X))}
# ################ Beta ################################
# # diff_coefmat = purrr::map(
# #   .x = seq_len(ncol(ridge1_coef)),
# #   .f = ~ sweep(x = ridge2_coef, MARGIN = 1,
# #                STATS = ridge1_coef[,.x, drop = FALSE],
# #                FUN = "-"))
#
# # diff_l2 = purrr::map(
# #   .x = diff_coefmat,
# #   .f = ~ col2norm(.x)) %>% do.call(rbind, .)
#
# # add_coefmat = purrr::map(
# #   .x = seq_len(ncol(ridge1_coef)),
# #   .f = ~ sweep(x = ridge2_coef, MARGIN = 1,
# #                STATS = ridge1_coef[,.x, drop = FALSE],
# #                FUN = "+"))
#
# # add_l2 = purrr::map(
# #   .x = add_coefmat,
# #   .f = ~ col2norm(.x)) %>% do.call(rbind, .)
#
# # criterion = purrr::map2(
# #   .x = diff_coefmat,
# #   .y = add_coefmat,
# #   .f = ~ col2norm(.x/.y)
# # ) %>% do.call(rbind, .)
# #
# # d3heatmap::d3heatmap(criterion, Rowv = F, Colv = F,
# #                      colors = "Blues")
#
#
# ################ Y ################################
# diff_coefmat = purrr::map(
#   .x = seq_len(ncol(pred_z1_beta1)),
#   .f = ~ sweep(x = pred_z1_beta2, MARGIN = 1,
#                STATS = pred_z1_beta1[,.x, drop = FALSE],
#                FUN = "-"))
#
# # diff_l2 = purrr::map(
# #   .x = diff_coefmat,
# #   .f = ~ col2norm(.x)) %>% do.call(rbind, .)
#
#
# add_coefmat = purrr::map(
#   .x = seq_len(ncol(pred_z1_beta1)),
#   .f = ~ sweep(x = pred_z1_beta2, MARGIN = 1,
#                STATS = pred_z1_beta1[,.x, drop = FALSE],
#                FUN = "+")/2)
#
# # add_l2 = purrr::map(
# #   .x = add_coefmat,
# #   .f = ~ col2norm(.x)) %>% do.call(rbind, .)
#
# criterion = purrr::map2(
#   .x = diff_coefmat,
#   .y = add_coefmat,
#   .f = ~ col1norm(.x/.y)
# ) %>% do.call(rbind, .)
#
# d3heatmap::d3heatmap(criterion, Rowv = F, Colv = F, colors = "Blues")


# criterion = (ridge1_coef - ridge2_coef)
# dim(criterion)
# d3heatmap::d3heatmap(criterion, Rowv = F, Colv = F)

# criterion = col2norm(pred_z1_beta1 - pred_z1_beta2)/sqrt(col2norm(pred_z1_beta1 + pred_z1_beta2))
# # criterion = col2norm(pred_z1_beta1 - pred_z1_beta2)^2/col2norm(pred_z1_beta1 + pred_z1_beta2)
# plot(criterion, type ="l")
# points(which.min(criterion), min(criterion), col = "red")
# which.min(criterion)

# pred_z1_beta1 = cbind(1, z1_reduced) %*% ridge1_coef
# pred_z1_beta2 = cbind(1, z1_reduced) %*% ridge2_coef
# pred_z2_beta1 = cbind(1, z2_reduced) %*% ridge1_coef
# pred_z2_beta2 = cbind(1, z2_reduced) %*% ridge2_coef
# beta1_l2_norm = ridge1_coef %>% col2norm
# beta2_l2_norm = ridge2_coef %>% col2norm
# diff_coefmat = purrr::map(
#   .x = seq_len(ncol(ridge1_coef)),
#   .f = ~ sweep(x = ridge2_coef, MARGIN = 1,
#                STATS = ridge1_coef[,.x, drop = FALSE],
#                FUN = "-"))
#
# diff_l2 = purrr::map(
#   .x = diff_coefmat,
#   .f = ~ apply(X = .x, MARGIN = 2, FUN = norm, type = "2")
# ) %>% do.call(rbind, .)
#

#
#
# (diff_l2/add_l2) %>% d3heatmap::d3heatmap(Rowv = F, Colv = F)
# coef2 = glmnet::coef.cv.glmnet(ridge2, s = s)[-1, , drop = FALSE]
# signCoef2 = sign(coef2)

