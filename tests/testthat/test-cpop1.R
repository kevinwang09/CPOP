context("Test output of CPOP 1")
data(cpop_data_binary, package = 'CPOP')
set.seed(1)
z1 = pairwise_col_diff(x1)
z2 = pairwise_col_diff(x2)
w = compute_weights(z1, z2)
nIter = 20
alpha = 0.1
s = "lambda.min"
cpop1_res = cpop1(z1, z2, y1, y2, w, nIter = 20, n_features = 20, alpha = alpha, s = "lambda.min", family = "binomial")
expect_identical(length(cpop1_res), 15L)
