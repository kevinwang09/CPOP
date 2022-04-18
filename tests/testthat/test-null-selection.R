context("Test output of CPOP model")
data(cpop_data_binary, package = 'CPOP')

x1 = cpop_data_binary$x1
x2 = cpop_data_binary$x2
set.seed(3)
y1 = rexp(nrow(x1), 1)
y2 = rexp(nrow(x2), 1)
expect_warning({
  cpop_model_result = cpop_model(x1 = x1, x2 = x2, y1 = y1, y2 = y2,
                                 alpha = 1, s = "lambda.min", family = "gaussian", intercept = TRUE)
  predict_cpop(cpop_model_result, newx = x1)
})
