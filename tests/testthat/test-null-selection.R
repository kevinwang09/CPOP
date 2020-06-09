context("Test output of CPOP model")
data(cpop_data_binary, package = 'CPOP')
attach(cpop_data_binary)
# set.seed(8) ## One single feature
set.seed(1)
z1 = pairwise_col_diff(x1)
z2 = pairwise_col_diff(x2)
y1 = rnorm(nrow(z1))
y2 = rnorm(nrow(z2))
expect_warning({
  cpop_model_result = cpop_model(z1 = z1, z2 = z2, y1 = y1, y2 = y2,
                                 alpha = 1, s = "lambda.min", family = "gaussian")
})
