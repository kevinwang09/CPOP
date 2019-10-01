#' @title Plot ratio network
#' @description  Plot ratio network
#' @param x The output of top1 or top2 or get_lasso_coef
#' @param nodesData Logical
#' @param stat_df stat_df
#' @importFrom tibble tibble
#' @importFrom tidyr separate
#' @importFrom visNetwork visNetwork
#' @import dplyr
#' @export
#' @examples
#' x = c("X1--X10","X1--X3","X2--X4","X1--X4","X1--X8","X1--X9",
#' "X10--X2","X2--X5","X2--X7","X2--X6")
#' plot_ratio_network(x, nodesData = FALSE) %>%
#' visNetwork::visPhysics(solver = "forceAtlas2Based",
#' forceAtlas2Based = list(gravitationalConstant = -100))
#' plot_ratio_network(x, nodesData = TRUE)
plot_ratio_network = function(x, nodesData = FALSE, stat_df = NULL){
  if(is.character(x)){
    edges = tibble::tibble(x) %>%
      tidyr::separate(col = x,
                      into = c("from", "to"),
                      sep = "--")

    nodes = tibble::tibble(id = unlist(edges)) %>%
      dplyr::group_by(id) %>%
      dplyr::summarise(
        nNeighbours = n(),
        size = 10*nNeighbours,
        font.size = 0.5*size) %>%
      dplyr::mutate(label = id)
  } else {
    stopifnot(!is.null(stat_df))

    edges = x %>%
      tidyr::separate(col = feature_name,
                      into = c("from", "to"),
                      sep = "--") %>% as_tibble() %>%
      dplyr::mutate(
        color = "gray",
        value = 10*beta,
        title = signif(beta, 2))

    nodes = tibble::tibble(id = c(edges$from, edges$to)) %>%
      dplyr::group_by(id) %>%
      dplyr::summarise(nNeighbours = n()) %>%
      left_join(stat_df, by = "id") %>%
      dplyr::mutate(label = id)
  }

  if(!nodesData){
    visNetwork::visNetwork(nodes, edges)
  } else {
    return(nodes)
  }
}

