context("Test output of CPOP model")
data(cpop_data_binary, package = 'CPOP')

x1 = cpop_data_binary$x1
x2 = cpop_data_binary$x2
x3 = cpop_data_binary$x3
y1 = cpop_data_binary$y1
y2 = cpop_data_binary$y2
y3 = cpop_data_binary$y3

z1 = pairwise_col_diff(x1)
z2 = pairwise_col_diff(x2)

set.seed(1)
cpop_model_result_x = cpop_model(x1 = x1, x2 = x2, y1 = y1, y2 = y2,
                               alpha = 1, s = "lambda.min")

cpop_model_result_z = cpop_model(z1 = z1, z2 = z2, y1 = y1, y2 = y2,
                               alpha = 1, s = "lambda.min")
