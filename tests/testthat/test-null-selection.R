context("Test output of CPOP model")
data(cpop_data_binary, package = 'CPOP')

x1 = cpop_data_binary$x1
x2 = cpop_data_binary$x2
# set.seed(8) ## One single feature
set.seed(1)
y1 = rnorm(nrow(x1))
y2 = rnorm(nrow(x2))
expect_warning({
  cpop_model_result = cpop_model(x1 = x1, x2 = x2, y1 = y1, y2 = y2,
                                 alpha = 1, s = "lambda.min", family = "gaussian")
})
