#' @title Compare pairwise diff
#' @description Compare pairwise diff
#' @param x A data matrix
#' @return A pairwise difference matrix
#' @importFrom magrittr set_colnames
#' @importFrom magrittr %>%
#' @importFrom purrr map
#' @importFrom purrr imap
#' @export
#' @examples
#' n = 1
#' p = 4
#' x = matrix(rep(1:p, n), nrow = n, ncol = p, byrow = TRUE)
#' colnames(x) = paste0("X", 1:p)
#' pairwise_col_diff(x)
pairwise_col_diff = function(x){
  x = x[,sort(colnames(x)), drop = FALSE]

  p = ncol(x)
  list_mat = purrr::map(
    .x = seq_len(p-1),
    .f = ~ x[,.x] - x[,-c(seq_len(.x)), drop = FALSE]
  )

  names(list_mat) = colnames(x)[-length(colnames(x))]


  list_mat_named = purrr::imap(
    .x = list_mat,
    .f = function(x, y){
      mat_name = paste0(y, "--", colnames(x))
      colnames(x) = mat_name
      return(x)
    })

  result = do.call(cbind, list_mat_named)

  return(result)
}
