#' @title Plot a ratio network
#' @description  Plot ratio network
#' @param x A character vector representing edges by separating nodes with "--"
#' i.e. in the form of "e1--e2"
#' @param type Type of graphical visualisation, currently only support one of
#' "visNetwork" (interactive graph).
#' \itemize{
#' \item "ggraph" (default): a non-interactive graph visualisation
#' using the `ggraph` package.
#' \item "igraph": a `igraph` object from the `igraph` package.
#' \item "visNetwork": an interactive graph visualisation using the `visNetwork` package
#' }
#' @import dplyr
#' @import ggraph
#' @importFrom igraph graph_from_edgelist
#' @importFrom stringr str_detect
#' @rdname cpop_network
#' @export
#' @examples
#' x = c("X1--X10","X1--X3", "X1--X4","X1--X8","X1--X9","X2--X5","X2--X7","X2--X6", "X8--X10")
#' plot_lratio_network(x, type = "ggraph")
#' plot_lratio_network(x, type = "igraph")
#' plot_lratio_network(x, type = "visNetwork")

plot_lratio_network = function(x, type = "ggraph"){
  assertthat::assert_that(type %in% c("visNetwork", "igraph", "ggraph"),
                          msg = "Only ggraph, visNetwork and igraph visualisations are supported")
  assertthat::assert_that(all(stringr::str_detect(x, "--")),
                          msg = "All edges must use the separator of '--'")

  network = lratio_to_network(x)
  edges = network$edges
  nodes = network$nodes

  if(type == "visNetwork"){
    assertthat::assert_that(requireNamespace("visNetwork"),
                            msg = "You need to install the visNetwork package")

    nodes = nodes %>% dplyr::mutate(
      label = id,
      font.size = 20)

    return(visNetwork::visNetwork(nodes, edges))
  }

  if(type == "igraph"){
    edges_mat = edges %>% as.data.frame %>% as.matrix
    ig = igraph::graph_from_edgelist(edges_mat, directed = FALSE)
    return(ig)
  }

  if(type == "ggraph"){
    assertthat::assert_that(requireNamespace("ggraph"),
                            msg = "You need to install the ggraph package")
    edges_mat = edges %>% as.data.frame %>% as.matrix
    ig = igraph::graph_from_edgelist(edges_mat, directed = FALSE)

    p = ggraph(ig, layout = "linear", circular = TRUE) +
      ggraph::geom_edge_arc(aes(
        start_cap = label_rect(.data$node1.name),
        end_cap = label_rect(.data$node2.name))) +
      ggraph::geom_node_text(aes(label = .data$name), size = 6)

    return(p)
  }

}


#' @title Make a network from lratio
#' @param x A character vector representing edges by separating nodes with "--"
#' i.e. in the form of "e1--e2"
#' @importFrom tibble tibble
#' @importFrom tidyr separate
#' @rdname cpop_network
#' @import dplyr
#' @export
lratio_to_network = function(x){
  assertthat::assert_that(is.character(x))

  edges = tibble::tibble(x) %>%
    tidyr::separate(col = x,
                    into = c("from", "to"),
                    sep = "--")

  nodes = tibble::tibble(id = unlist(edges),
                         label = id) %>%
    dplyr::group_by(id) %>%
    dplyr::tally(name = "n_neighbours") %>%
    dplyr::ungroup()

  return(lst(edges, nodes))
}

#' @title Make a network from lratio
#' @param x A character vector representing edges by separating nodes with "--"
#' i.e. in the form of "e1--e2"
#' @importFrom igraph graph_from_edgelist mst get.edgelist
#' @rdname cpop_network
mst_lratio = function(x){
  network = lratio_to_network(x = x)
  edges_mat = network$edges %>% as.data.frame %>% as.matrix
  ig = igraph::graph_from_edgelist(edges_mat, directed = FALSE)
  ig_mst = igraph::mst(ig) ## minimum spanning tree removes collinearity
  ig_mst_edges = igraph::get.edgelist(ig_mst)
  ig_mst_edges = apply(ig_mst_edges, 1, sort)
  result = paste0(ig_mst_edges[1,], "--", ig_mst_edges[2,])
  return(result)
}
