#' @title Plot cpop coefficients
#' @description Plot cpop coefficients
#' @param cpop_result The output of cpop_model
#' @param s lasso s
#' @param type One of "point", "bar", "text" and "ggraph"
#' @import ggplot2
#' @importFrom tibble tibble
#' @importFrom tidyr gather
#' @importFrom dplyr mutate
#' @importFrom forcats fct_reorder
#' @import ggraph
#' @export
#' @examples
#' data(cpop_data_binary, package = 'CPOP')
#' set.seed(1)
#' z1 = pairwise_col_diff(x1)
#' z2 = pairwise_col_diff(x2)
#' cpop_result = cpop_model(z1, z2, y1, y2, alpha = 1, n_features = 10)
#' plot_cpop(cpop_result, type = "point")
#' plot_cpop(cpop_result, type = "text")
#' plot_cpop(cpop_result, type = "bar")
#' plot_cpop(cpop_result, type = "ggraph")
plot_cpop <- function(cpop_result, type = "point", s = "lambda.min"){
  # assertthat::assert_that("cpop" %in% class(cpop_result),
  #                         msg = "The input object must be a cpop class object")

  assertthat::assert_that(type %in% c("point", "text", "bar", "ggraph"),
                          msg = "Only ggraph, visNetwork and igraph visualisations are supported")
  coef1 = glmnet::coef.glmnet(cpop_result$glmnet1, s = s)
  coef2 = glmnet::coef.glmnet(cpop_result$glmnet2, s = s)

  stopifnot(identical(rownames(coef1),
                      rownames(coef2)))

  coef_plotdf = tibble::tibble(coef_name = rownames(coef1),
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

    return(lst(plot = g1, data = coef_plotdf))
  }

  if(type == "bar"){
    coef_plotdf_gather = coef_plotdf %>%
      tidyr::gather(coef_key, coef_value, -coef_name)

    p = coef_plotdf_gather %>%
      ggplot(aes(x = coef_name, y = coef_value,
                 fill = coef_key)) +
      geom_col(position = "dodge") +
      theme(axis.text.x = element_text(angle = 90))

    return(lst(plot = p, data = coef_plotdf_gather))
  }

  if(type == "text"){
    p = ggplot(coef_plotdf,
                aes(x = coef1, y = coef2, label = coef_name)) +
      geom_text() +
      geom_abline(slope = 1, intercept = 0, colour = "red")

    return(lst(plot = p, data = coef_plotdf))
  }

  if(type == "ggraph"){
    assertthat::assert_that(requireNamespace("ggraph"),
                            msg = "You need to install the ggraph package")

    network_tbl = coef_plotdf %>%
      dplyr::mutate(coef_name = as.character(coef_name),
                    coef_avg = (coef1 + coef2)/2,
                    coef_abs = abs(coef_avg),
                    sign_coef1 = ifelse(coef_avg < 0, "Negative", "Positive")) %>%
      dplyr::filter(coef_name != "(Intercept)")

    edges_tbl = network_tbl %>%
      tidyr::separate(col = "coef_name", into = c("from", "to"))

    ig = igraph::graph_from_data_frame(edges_tbl, directed = FALSE)

    p = ggraph(ig, layout = "linear", circular = TRUE) +
      ggraph::geom_edge_arc(aes(
        start_cap = label_rect(node1.name),
        end_cap = label_rect(node2.name),
        width = coef_abs,
        colour = sign_coef1)) +
      ggraph::geom_node_text(aes(label = name), size = 6) +
      ggraph::scale_edge_colour_brewer(palette = "Set1", direction = -1)

    return(lst(plot = p, data = edges_tbl))
  }
}
