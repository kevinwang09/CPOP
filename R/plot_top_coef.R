#' @title Plot top coefficients
#' @description Plot top coefficients
#' @param top_model_result The output of top_model
#' @param s laso s
#' @import ggplot2
#' @importFrom tibble tibble
#' @importFrom tidyr gather
#' @importFrom dplyr mutate
#' @importFrom forcats fct_reorder
#' @export
#' @examples
#' set.seed(1)
#' n = 100
#' p = 10
#' x1 = matrix(rnorm(n * p, mean = 0, sd = 1), nrow = n, ncol = p)
#' x2 = x1 + 0.1
#' colnames(x1) = colnames(x2) = paste0("X", 1:p)
#' k = 2
#' beta = c(rep(1, k), rep(0, p - k))
#' expit = function(x) 1/(1+exp(-x))
#' y1 = rbinom(n, 1, prob = expit(x1 %*% beta))
#' y2 = rbinom(n, 1, prob = expit(x2 %*% beta))
#' z1 = pairwise_col_diff(x1)
#' z2 = pairwise_col_diff(x2)
#' w = compute_weights(z1, z2)
#' top_model_result = top_model(z1, z2, y1, y2, w = w,
#' alpha = 1, n_features = 40, s = "lambda.min")
plot_top_coef = function(top_model_result, s = "lambda.min"){
  top_model_result$en1$lambda.min
  top_model_result$en2$lambda.min
  coef_en1 = get_lasso_coef(top_model_result$en1, s = "lambda.min")
  coef_en2 = get_lasso_coef(top_model_result$en2, s = "lambda.min")

  stopifnot(identical(rownames(coef_en1),
                      rownames(coef_en2)))

  coef_plotdf = tibble(coef_name = rownames(coef_en1),
                       coef_en1,
                       coef_en2) %>%
    dplyr::mutate(
      coef_name = forcats::fct_reorder(coef_name, coef_en1)
    )

  ggplot(coef_plotdf,
         aes(x = coef_en1, coef_en2)) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0, colour = "red")


  coef_plotdf_gather = coef_plotdf %>%
    tidyr::gather(coef_key, coef_value, -coef_name)

  coef_plotdf_gather %>%
    ggplot(aes(x = coef_name, y = coef_value,
               fill = coef_key)) +
    geom_col()

}
