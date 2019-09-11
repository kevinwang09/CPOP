#' @title Plot cpop coefficients
#' @description Plot cpop coefficients
#' @param cpop_model_result The output of cpop_model
#' @param s laso s
#' @param type "scatter" or "bar"
#' @import ggplot2
#' @importFrom tibble tibble
#' @importFrom tidyr gather
#' @importFrom dplyr mutate
#' @importFrom forcats fct_reorder
#' @export
#' @examples
#' data(cpop_data, package = 'CPOP')
#' set.seed(1)
#' z1 = pairwise_col_diff(x1)
#' z2 = pairwise_col_diff(x2)
#' w = compute_weights(z1, z2)
#' cpop_model_result = cpop_model(z1, z2, y1, y2, w = w,
#' alpha = 1, n_features = 40, s = "lambda.min")
#' alpha = c(1, 0.1)
#' cpop_model_result = cpop_model(z1, z2, y1, y2, w = w,
#' cpop1_iterate = TRUE, alpha = alpha, n_features = 40, s = "lambda.min")
#' plot_cpop_coef(cpop_model_result, s = "lambda.min", type = "point")
#' plot_cpop_coef(cpop_model_result, s = "lambda.min", type = "text")
#' plot_cpop_coef(cpop_model_result, s = "lambda.min", type = "bar")
plot_cpop_coef = function(cpop_model_result, s = "lambda.min", type = "point"){
  coef_en1 = get_lasso_coef(cpop_model_result$en1, s = s)
  coef_en2 = get_lasso_coef(cpop_model_result$en2, s = s)

  stopifnot(identical(rownames(coef_en1),
                      rownames(coef_en2)))

  coef_plotdf = tibble(coef_name = rownames(coef_en1),
                       coef_en1,
                       coef_en2) %>%
    dplyr::mutate(
      coef_name = forcats::fct_reorder(coef_name, coef_en1)
    )

  if(type == "point"){
    g1 = ggplot(coef_plotdf,
                aes(x = coef_en1, y = coef_en2)) +
      geom_point() +
      geom_abline(slope = 1, intercept = 0, colour = "red")

    return(g1)

  }

  if(type == "bar"){
    coef_plotdf_gather = coef_plotdf %>%
      tidyr::gather(coef_key, coef_value, -coef_name)

    g2 = coef_plotdf_gather %>%
      ggplot(aes(x = coef_name, y = coef_value,
                 fill = coef_key)) +
      geom_col(position = "dodge")
    return(g2)
  }

  if(type == "text"){
    g3 = ggplot(coef_plotdf,
                aes(x = coef_en1, y = coef_en2, label = coef_name)) +
      geom_text() +
      geom_abline(slope = 1, intercept = 0, colour = "red")

    return(g3)
  }

}
