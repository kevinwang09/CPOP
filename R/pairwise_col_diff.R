#' @title Compare pairwise diff
#' @description Compare pairwise diff
#' @param x A data matrix
#' @return A pairwise difference matrix
#' @importFrom magrittr set_colnames
#' @importFrom magrittr %>%
#' @importFrom purrr map
#' @importFrom purrr map2
#' @export
#' @examples
#' n = 10
#' p = 10
#' x = matrix(rep(1:p, n), nrow = n, ncol = p, byrow = TRUE)
#' colnames(x) = paste0("X", 1:p)
#' pairwise_col_diff(x)
pairwise_col_diff = function(x){
  all_pair = utils::combn(colnames(x), 2) %>% t %>%
    magrittr::set_colnames(c("Gene1", "Gene2"))

  all_pair_names = paste(all_pair[,"Gene1"], all_pair[,"Gene2"], sep = "--")

  res = form_pairs(all_pair = all_pair,
                   data = x,
                   all_pair_names = all_pair_names)
  return(res)
}


form_pairs = function(all_pair, data, all_pair_names){
  diff_mat = purrr::map2(
    .x = all_pair[,"Gene1"],
    .y = all_pair[,"Gene2"],
    .f = ~ data[,.x] - data[,.y])
  names(diff_mat) = all_pair_names
  res = do.call(cbind, diff_mat)
  return(res)
}
