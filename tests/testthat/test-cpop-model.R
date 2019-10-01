context("Test output of CPOP model")
data(cpop_data_binary, package = 'CPOP')
set.seed(1)
z1 = pairwise_col_diff(x1)
z2 = pairwise_col_diff(x2)
w = compute_weights(z1, z2)
cpop_model_result = cpop_model(z1 = z1, z2 = z2, y1 = y1, y2 = y2, w = w,
                               alpha = 1, s = "lambda.min")


boxplot(cbind(x1, x2))
boxplot(cbind(z1, z2))
plot(coef(cpop_model_result$glmnet1), coef(cpop_model_result$glmnet2))
plot(colMeans(x1), colMeans(x2))
plot(colMeans(z1), colMeans(z2))
cor(colMeans(z1), colMeans(z2))

plot_glmnet_coef(cpop_model_result, type = "text")
plot(predict_cpop(cpop_model_result, newz = z1, model_number = "both", tibble = TRUE)[,c(2,3)])
abline(a = 0, b = 1, col = "red")
plot(predict_cpop(cpop_model_result, newz = z2, model_number = "both", tibble = TRUE)[,c(2,3)])
abline(a = 0, b = 1, col = "red")
#
# tmp = naive_glmnet(z1 = z1, z2 = z2, y1 = y1, y2 = y2, family = "binomial", alpha = 1)
# get_lasso_coef(tmp$glmnet1, s = "lambda.min")
# get_lasso_coef(tmp$glmnet2, s = "lambda.min")
# plot(predict(tmp$glmnet1, newx = z1),
#      predict(tmp$glmnet2, newx = z1))
# abline(a = 0, b = 1, col = "red")

