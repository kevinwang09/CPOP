context("Test output of CPOP model")
data(cpop_data_binary, package = 'CPOP')

x1 = cpop_data_binary$x1
x2 = cpop_data_binary$x2
x3 = cpop_data_binary$x3
y1 = cpop_data_binary$y1
y2 = cpop_data_binary$y2
y3 = cpop_data_binary$y3

# set.seed(8) ## One single feature
set.seed(1)
y1 = rnorm(nrow(z1))
y2 = rnorm(nrow(z2))
expect_warning({
  cpop_model_result = cpop_model(x1 = x1, x2 = x2, y1 = y1, y2 = y2,
                                 alpha = 1, s = "lambda.min", family = "gaussian")
})
