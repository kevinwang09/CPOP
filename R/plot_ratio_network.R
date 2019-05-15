#' @title Plot ratio network
#' @description  Plot ratio network
#' @param x The output of top1 or top2
#' @param nodesData Logical
#' @importFrom tibble tibble
#' @importFrom tidyr separate
#' @importFrom visNetwork visNetwork
#' @import dplyr
#' @export
#' @examples
#' x = c("X1--X2","X1--X3","X1--X4","X1--X5","X1--X6","X1--X7","X1--X8",
#' "X1--X9","X1--X10","X2--X3","X2--X4","X2--X5","X2--X6","X2--X7",
#' "X2--X8","X2--X9","X2--X10","X3--X4","X3--X7","X3--X8","X3--X9",
#' "X4--X5","X4--X7","X4--X9","X5--X10","X7--X8","X7--X9","X8--X9")
#' plot_ratio_network(x, nodesData = FALSE)
#' plot_ratio_network(x, nodesData = TRUE)
plot_ratio_network = function(x, nodesData = FALSE){
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

  if(!nodesData){
    visNetwork::visNetwork(nodes, edges)
  } else {
    return(nodes)
  }
}

