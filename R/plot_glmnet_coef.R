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
#' data(cpop_data_binary, package = 'CPOP')
#' set.seed(1)
#' z1 = pairwise_col_diff(x1)
#' z2 = pairwise_col_diff(x2)
#' w = compute_weights(z1, z2)
#' cpop_model_result = cpop_model(z1, z2, y1, y2, w = w,
#' alpha = 1, n_features = 40, s = "lambda.min")
#' plot_glmnet_coef(cpop_model_result, s = "lambda.min", type = "point")
#' plot_glmnet_coef(cpop_model_result, s = "lambda.min", type = "text")
#' plot_glmnet_coef(cpop_model_result, s = "lambda.min", type = "bar")
plot_glmnet_coef = function(model_result, s = "lambda.min", type = "point"){

  if("cv.glmnet" %in% class(model_result$glmnet1)){
    coef1 = glmnet::coef.cv.glmnet(model_result$glmnet1, s = s)
    coef2 = glmnet::coef.cv.glmnet(model_result$glmnet2, s = s)
  } else if("glmnet" %in% class(model_result$glmnet1)){
    coef1 = glmnet::coef.glmnet(model_result$glmnet1, s = s)
    coef2 = glmnet::coef.glmnet(model_result$glmnet2, s = s)
  } else {
    stop("Only glmnet and cv.glmnet objects are acceptable")
  }

  stopifnot(identical(rownames(coef1),
                      rownames(coef2)))

  coef_plotdf = tibble(coef_name = rownames(coef1),
                       coef1 = as.vector(coef1),
                       coef2 = as.vector(coef2)) %>%
    dplyr::mutate(
      coef_name = forcats::fct_reorder(coef_name, coef1)
    )

  if(type == "point"){
    g1 = ggplot(coef_plotdf,
                aes(x = coef1, y = coef2)) +
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
                aes(x = coef1, y = coef2, label = coef_name)) +
      geom_text() +
      geom_abline(slope = 1, intercept = 0, colour = "red")

    return(g3)
  }

}
