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
#' @importFrom rlang .data
#' @import ggraph
#' @export
#' @examples
#' data(cpop_data_binary, package = 'CPOP')
#' ## Loading simulated matrices and vectors
#' x1 = cpop_data_binary$x1
#' x2 = cpop_data_binary$x2
#' y1 = cpop_data_binary$y1
#' y2 = cpop_data_binary$y2
#' set.seed(1)
#' cpop_result = cpop_model(x1 = x1, x2 = x2, y1 = y1, y2 = y2, alpha = 1, n_features = 10)
#' plot_cpop(cpop_result, type = "point")
#' plot_cpop(cpop_result, type = "text")
#' plot_cpop(cpop_result, type = "bar")
#' plot_cpop(cpop_result, type = "ggraph")
plot_cpop <- function(cpop_result, type = "point", s = "lambda.min"){
  assertthat::assert_that(type %in% c("point", "text", "bar", "ggraph"),
                          msg = "Only ggraph, visNetwork and igraph visualisations are supported")
  coef1 = glmnet::coef.glmnet(cpop_result$glmnet1, s = s)
  coef2 = glmnet::coef.glmnet(cpop_result$glmnet2, s = s)

  stopifnot(identical(rownames(coef1),
                      rownames(coef2)))

  coef_plotdf = tibble::tibble(coef_name = rownames(coef1),
                       coef1 = as.vector(coef1),
                       coef2 = as.vector(coef2)) %>%
    dplyr::mutate(coef_name = forcats::fct_reorder(.data$coef_name, coef1)) %>%
    dplyr::filter(coef1 != 0, coef2 != 0)

  if(type == "point"){
    g1 = ggplot(coef_plotdf,
                aes(x = .data$coef1, y = .data$coef2)) +
      geom_point() +
      geom_abline(slope = 1, intercept = 0, colour = "red")

    return(lst(plot = g1, data = coef_plotdf))
  }

  if(type == "bar"){
    coef_plotdf_gather = tidyr::pivot_longer(
      data = coef_plotdf, cols = c("coef1", "coef2"),
      names_to = "coef_key", values_to = "coef_value")
      # tidyr::gather(coef_key, coef_value, -coef_name)

    p = coef_plotdf_gather %>%
      ggplot(aes(x = .data$coef_name, y = .data$coef_value,
                 fill = .data$coef_key)) +
      geom_col(position = "dodge") +
      theme(axis.text.x = element_text(angle = 90))

    return(lst(plot = p, data = coef_plotdf_gather))
  }

  if(type == "text"){
    p = ggplot(coef_plotdf,
                aes(x = .data$coef1, y = .data$coef2, label = .data$coef_name)) +
      geom_text() +
      geom_abline(slope = 1, intercept = 0, colour = "red")

    return(lst(plot = p, data = coef_plotdf))
  }

  if(type == "ggraph"){
    assertthat::assert_that(requireNamespace("ggraph"),
                            msg = "You need to install the ggraph package")

    network_tbl = coef_plotdf %>%
      dplyr::mutate(coef_name = as.character(.data$coef_name),
                    coef_avg = (.data$coef1 + .data$coef2)/2,
                    coef_abs = abs(.data$coef_avg),
                    sign_coef1 = ifelse(.data$coef_avg < 0, "Negative", "Positive")) %>%
      dplyr::filter(.data$coef_name != "(Intercept)")

    edges_tbl = network_tbl %>%
      tidyr::separate(col = "coef_name", into = c("from", "to"))

    ig = igraph::graph_from_data_frame(edges_tbl, directed = FALSE)

    p = ggraph(ig, layout = "linear", circular = TRUE) +
      ggraph::geom_edge_arc(aes(
        start_cap = label_rect(.data$node1.name),
        end_cap = label_rect(.data$node2.name),
        width = .data$coef_abs,
        colour = .data$sign_coef1)) +
      ggraph::geom_node_text(aes(label = .data$name), size = 6) +
      ggraph::scale_edge_colour_brewer(palette = "Set1", direction = -1)

    return(lst(plot = p, data = edges_tbl))
  }
}
