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
#' x = c("X1--X2","X1--X3","X1--X4","X1--X5","X1--X6","X1--X7","X1--X8",
#' "X1--X9","X1--X10","X2--X3","X2--X4","X2--X5","X2--X6","X2--X7",
#' "X2--X8","X2--X9","X2--X10","X3--X4","X3--X7","X3--X8","X3--X9",
#' "X4--X5","X4--X7","X4--X9","X5--X10","X7--X8","X7--X9","X8--X9")
#' plot_ratio_network(x, nodesData = FALSE)
#' plot_ratio_network(x, nodesData = TRUE)
#' set.seed(1)
#' n = 100
#' p = 10
#' x1 = matrix(rnorm(n * p, mean = 0, sd = 1), nrow = n, ncol = p)
#' x2 = x1 + 0.1
#' colnames(x1) = colnames(x2) = paste0("X", 1:p)
#' k = 2
#' beta = c(rep(c(-1, 1), length.out = k), rep(0, p - k))
#' expit = function(x) 1/(1+exp(-x))
#' y1 = rbinom(n, 1, prob = expit(x1 %*% beta))
#' y2 = rbinom(n, 1, prob = expit(x2 %*% beta))
#' z1 = pairwise_col_diff(x1)
#' z2 = pairwise_col_diff(x2)
#' w = compute_weights(z1, z2)
#' top_model_result = top_model(z1, z2, y1, y2, w = w,
#' alpha = 1, n_features = 40, s = "lambda.min")
#' top_model_coef = get_lasso_coef(lassoObj = top_model_result$en1, s = "lambda.min", tibble = TRUE)
#' stat_df = tibble::tibble(
#' id = colnames(x1),
#' stat = beta,
#' size = 10*abs(stat),
#' color = ifelse(stat < 0, "blue", "red"),
#' font.size = ifelse(abs(stat) <= 0.5, 30, 60),
#' title = signif(stat, 2)
#' )
#' plot_ratio_network(x = top_model_coef, stat_df = stat_df)
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

