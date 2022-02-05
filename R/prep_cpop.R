#' @title Compute pairwise difference between matrix columns
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
  assertthat::assert_that(!is.null(colnames(x)))
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
#' @description Checks before CPOP modelling, and prepare the z-matrices for CPOP modelling.
#' @param x1 A data matrix of size n times p
#' @param x2 A data matrix of size n times p
#' @param force If the data matrices has too many columns (p > 200), then the
#' function will not compute output the
#' z-matrices (pairwise column differences of x's). Default to FALSE.
#' @rdname prep_cpop
#' @export
#' @examples
#' data(cpop_data_binary, package = 'CPOP')
#' ## Loading simulated matrices and vectors
#' x1 = cpop_data_binary$x1
#' x2 = cpop_data_binary$x2
#' prep_cpop(x1 = x1, x2 = x2)
prep_cpop = function(x1, x2, force = FALSE){
  assertthat::assert_that(ncol(x1) == ncol(x2))
  assertthat::assert_that(identical(colnames(x1), colnames(x2)))

  if(ncol(x1) < 1000 | force){
    z1 = pairwise_col_diff(x1)
    z2 = pairwise_col_diff(x2)
    return(list(z1 = z1, z2 = z2))
  } else {
    stop("Number of columns in x1 and x2 is larger than 1000. CPOP could take a long time to compute.
         \n Set `force = TRUE` to enforce computation.")
  }
}
