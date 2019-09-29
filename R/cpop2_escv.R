#' @title Step 2 of the CPOP method using HDCI package
#' @description Step 2 of the CPOP method using HDCI package
#' @param cpop1_result cpop1 result
#' @param z1 A data matrix
#' @param z2 A data matrix
#' @param y1 A vector
#' @param y2 A vector
#' @param nIter Number of iterations
#' @param s CV-Lasso lambda
#' @param ... Extra parameter settings for cv.glmnet
#' @param family see glmnet family
#' @param cpop2_break Should cpop2 loop be broken the first time
#' differential betas are removed
#' differential betas are removed
#' @importFrom glmnet cv.glmnet
#' @importFrom glmnet coef.cv.glmnet
#' @importFrom HDCI escv.glmnet
#' @return A vector
#' @export
#' @examples
#' data(cpop_data_binary, package = 'CPOP')
#' set.seed(1)
#' z1 = pairwise_col_diff(x1)
#' z2 = pairwise_col_diff(x2)
#' w = compute_weights(z1, z2)
#' cpop1_result = cpop1_iterate(
#' z1 = z1, z2 = z2, y1 = y1, y2 = y2, w = w,
#' family = "binomial", alpha = 0.1)
#' cpop2_hdci_result = cpop2_hdci(z1 = z1, z2 = z2, y1 = y1, y2 = y2,
#' cpop1_result = cpop1_result, family = "binomial")

cpop2_hdci = function(z1, z2, y1, y2, cpop1_result, family,
                      s = "lambda.min", nIter = 20,
                      cpop2_break = TRUE, ...){
  p = length(cpop1_result)
  cpop2_features = cpop1_result

  for(j in 1:nIter){
    z1_reduced = z1[,cpop2_features]
    z2_reduced = z2[,cpop2_features]

    ridge1 = HDCI::escv.glmnet(
      x = z1_reduced,
      y = y1,
      family = family,
      alpha = 0, intercept = FALSE, ...)

    coef1 = glmnet::coef.cv.glmnet(ridge1, s = ridge1$lambda.escv)[-1, , drop = FALSE]
    signCoef1 = sign(coef1)

    ridge2 =  HDCI::escv.glmnet(
      x = z2_reduced,
      y = y2,
      family = family,
      alpha = 0, intercept = FALSE, ...)

    coef2 = glmnet::coef.cv.glmnet(ridge2, s = ridge2$lambda.escv)[-1, , drop = FALSE]
    signCoef2 = sign(coef2)

    cpop2_features = cpop2_features[as.matrix(signCoef1 == signCoef2)]

    confTable = table(
      factor(as.matrix(signCoef1), levels = c(-1, 0, 1)),
      factor(as.matrix(signCoef2), levels = c(-1, 0, 1)))
    confTable_diag0 = confTable
    diag(confTable_diag0) = 0
    message("CPOP2 - Sign: Step ", sprintf("%02d", j), ": Number of leftover features: ", length(cpop2_features), " out of ", p)
    message("The sign matrix between the two data:")
    print(confTable_diag0)
    if(cpop2_break & sum(confTable_diag0) == 0){break}
  } ## End j-loop
  return(cpop2_features)
}
####################################################
####################################################
####################################################





####################################################
####################################################
####################################################
# set.seed(1)
# n = 1000
# p = 10
# x1 = matrix(rnorm(n * p, mean = 0, sd = 1), nrow = n, ncol = p)
# x2 = x1 + matrix(rnorm(n * p, mean = 1, sd = 0.1), nrow = n, ncol = p)
# colnames(x1) = colnames(x2) = paste0("X", 1:p)
# rbind(x1, x2) %>% rowMeans() %>% plot
# k = 2
# beta = c(rep(1, k), rep(0, p - k))
# expit = function(x) 1/(1+exp(-x))
# y1 = rbinom(n, 1, prob = expit(x1 %*% beta))
# y2 = rbinom(n, 1, prob = expit(x2 %*% beta))
# z1 = pairwise_col_diff(x1)
# z2 = pairwise_col_diff(x2)
# rbind(z1, z2) %>% rowMeans() %>% plot
# all.equal(x1, x2)
# all.equal(z1, z2)
# w = compute_weights(z1, z2)
# nIter = 20
# alpha = 0.1
# s = "lambda.min"
# cpop1_result = cpop1(z1, z2, y1, y2, w, nIter = 20, n_features = 50, alpha = alpha, s = "lambda.min")
# # top2_result = top2_sign(z1, z2, y1, y2, top1_result = top1_result, s = "lambda.min", nIter = 20)
#
#
# p = length(cpop1_result)
# cpop2_features = cpop1_result
# lambda = 10^seq(2,-3,length=100)
#
# z1_reduced = z1[,cpop2_features]
# z2_reduced = z2[,cpop2_features]
# all.equal(z1_reduced, z2_reduced)
# rbind(z1_reduced, z2_reduced) %>% boxplot
#
#
# ridge1 = glmnet::cv.glmnet(
#   x = z1_reduced,
#   y = y1,
#   family = "binomial",
#   alpha = 0, lambda = lambda, intercept = FALSE)
#
# ridge2 = glmnet::cv.glmnet(
#   x = z2_reduced,
#   y = y2,
#   family = "binomial",
#   alpha = 0, lambda = lambda, intercept = FALSE)
#
# which(lambda == ridge1$lambda.min)
# which(lambda == ridge2$lambda.min)
#
# ridge1_coef = coef(ridge1, s = lambda)
# ridge2_coef = coef(ridge2, s = lambda)
# dim(ridge1_coef)
# dim(ridge2_coef)
#
# plot(get_lasso_coef(ridge1, s = "lambda.1se"),
#      get_lasso_coef(ridge2, s = "lambda.1se"))
# abline(a = 0, b = 1, col = "red")
#
#
# ## Prediction values for every beta values
# pred_z1_beta1 = cbind(1, z1_reduced) %*% ridge1_coef
# pred_z1_beta2 = cbind(1, z1_reduced) %*% ridge2_coef
# pred_z2_beta1 = cbind(1, z2_reduced) %*% ridge1_coef
# pred_z2_beta2 = cbind(1, z2_reduced) %*% ridge2_coef
#
# all.equal(pred_z1_beta1, pred_z1_beta2)
# all.equal(pred_z2_beta1, pred_z2_beta2)
#
#
# col2norm = function(X){apply(X = X, MARGIN = 2, FUN = base::norm, type = "2")}
# col1norm = function(X){Matrix::colSums(abs(X))}
#
# ################################################
# # test_index_z1 = caret::createFolds(y1, k = 10)
# # pred_z1_beta1_test = purrr::map(test_index_z1, ~pred_z1_beta1[.x, ])
# # pred_z1_beta2_test = purrr::map(test_index_z1, ~pred_z1_beta2[.x, ])
# # test_index_z2 = caret::createFolds(y2, k = 10)
# # pred_z2_beta1_test = purrr::map(test_index_z2, ~pred_z2_beta1[.x, ])
# # pred_z2_beta2_test = purrr::map(test_index_z2, ~pred_z2_beta2[.x, ])
# # t.test(pred_z1_beta1_test[[1]][,1], pred_z1_beta2_test[[1]][,1])
# ################################################
# one_vs_mat_ttest = function(x, mat){
#   purrr::map_dbl(seq_along(lambda),
#              ~ tryCatch(t.test(x, mat[,.x])$statistic, error = function(e){1000}))
# }
#
#
#
# pred_z1_tstat = purrr::map(seq_along(lambda),
#                            ~ one_vs_mat_ttest(pred_z1_beta1[,.x], pred_z1_beta2)) %>%
#   do.call(cbind, .) %>% abs()
#
# dim(pred_z1_tstat)
#
# pred_z1_tstat %>%
#   d3heatmap::d3heatmap(Rowv = F, Colv = F, colors = "Blues")
# ############### If using usual CV method ##############################
# plot(pred_z1_tstat[,which(lambda == ridge1$lambda.min)])
#
# plot(pred_z1_beta1[,which(lambda == ridge1$lambda.min)], pred_z1_beta2[,which(lambda == ridge2$lambda.min)])
# abline(a = 0, b = 1, col = "red")
# t.test(pred_z1_beta1[,which(lambda == ridge1$lambda.min)], pred_z1_beta2[,which(lambda == ridge2$lambda.min)])
#
#
# ############### If using usual t-stat CV method ##############################
# tstat_stable_index = which(pred_z1_tstat == min(pred_z1_tstat), arr.ind = TRUE)
# plot(pred_z1_beta1[,tstat_stable_index[1]], pred_z1_beta2[,tstat_stable_index[2]])
# abline(a = 0, b = 1, col = "red")
# t.test(pred_z1_beta1[,tstat_stable_index[1]], pred_z1_beta2[,tstat_stable_index[2]])
# ################################################################
# new_iden_dist = function(x, y){
#   identityDist(x, y) +
#     abs(e1071::skewness(x, y)) +
#     (1 - cor(x, y))
# }
#
# one_vs_mat_newdist = function(x, mat){
#   purrr::map_dbl(seq_along(lambda),
#                  ~ tryCatch(new_iden_dist(x, mat[,.x]), error = function(e){10}))
# }
#
# pred_z1_newdist = purrr::map(seq_along(lambda),
#                            ~ one_vs_mat_newdist(pred_z1_beta1[,.x], pred_z1_beta2)) %>%
#   do.call(cbind, .) %>% abs
#
# pred_z1_newdist %>%
#   d3heatmap::d3heatmap(Rowv = F, Colv = F, colors = "Blues")
#
# newdist_stable_index = which(pred_z1_newdist == min(pred_z1_newdist), arr.ind = TRUE)
# plot(pred_z1_beta1[,newdist_stable_index[1]], pred_z1_beta2[,newdist_stable_index[2]])
# abline(a = 0, b = 1, col = "red")
