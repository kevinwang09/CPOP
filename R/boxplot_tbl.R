#' @title Make a simple boxplot plotting tbl
#' @description Make a simple boxplot plotting tbl
#' @param x a matrix or data.frame
#' @param index summarisation index
#' @importFrom tibble tibble
#' @importFrom forcats as_factor
#' @return A tibble
#' @export
#' @examples
#' data(cpop_data_binary, package = 'CPOP')
#' attach(cpop_data_binary)
#' set.seed(1)
#' z1 = pairwise_col_diff(x1)
#' z2 = pairwise_col_diff(x2)
#' boxplot_tbl(x = x1, index = 1)
#' boxplot_tbl(x = x1, index = 2)
boxplot_tbl = function(x, index = 2){
  means = apply(x, index, mean, na.rm = TRUE)
  medians = apply(x, index, median, na.rm = TRUE)
  q1 = apply(x, index, quantile, 0.25, na.rm = TRUE)
  q3 = apply(x, index, quantile, 0.75, na.rm = TRUE)
  n_na = apply(is.na(x), index, sum)

  object = dimnames(x)[[index]]

  if(is.null(object)){
    n = dim(x)[index]
    digits = ceiling(log10(n + 1L))
    fmt = paste0("%0", digits, "d")
    object = sprintf(fmt, seq_len(n))
  }

  object = forcats::as_factor(object)


  tbl = tibble(
    object = object,
    means, medians, q1, q3, n_na)

  return(tbl)
}
