context("Test output of CPOP 1")
data(cpop_data_binary, package = 'CPOP')
attach(cpop_data_binary)
set.seed(1)
z1 = pairwise_col_diff(x1)
z2 = pairwise_col_diff(x2)
cpop1_res = cpop1_iterate(z1 = z1, z2 = z2, y1 = y1, y2 = y2,
                          family = "binomial", alpha = 1)
