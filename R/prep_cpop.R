#' @title Preparation for CPOP modelling
#' @description Compute pairwise difference between matrix columns
#' @param x A data matrix of size n times p. Where rows are observations and
#' columns are features.
#' @return A matrix of size n times (p choose 2), where each column is the
#' difference between two of the original columns.
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

#' @title Preparation for CPOP modelling
#' @description Checks before CPOP modelling
#' @param x1 A data matrix
#' @param x2 A data matrix
#' @examples
#' n = 10
#' p = 10
#' x1 = matrix(rnorm(n * p, mean = 0, sd = 1), nrow = n, ncol = p)
#' x2 = matrix(rnorm(n * p, mean = 0, sd = 1), nrow = n, ncol = p)
#' prep_cpop(x1 = x1, x2 = x2)
prep_cpop = function(x1, x2){
  assertthat::assert_that(ncol(x1) == ncol(x2))

  x_plotdf = tibble::tibble(
    colnames = colnames(x1),
    x1_colmeans = colMeans(x1),
    x2_colmeans = colMeans(x2))

  ggplot(data = x_plotdf,
         mapping = aes(x = x1_colmeans,
                       y = x2_colmeans)) +
    geom_point()

  p = ncol(x)
  list_mat = purrr::map(
    .x = seq_len(p-1),
    .f = ~ x[,.x] - x[,-c(seq_len(.x)), drop = FALSE]
  )

}
