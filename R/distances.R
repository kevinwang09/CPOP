#' @title Identity distance between two vectors
#' @description Identity distance (`identity_dist`) between two vectors is
#' defined as: d = (x - y)/sqrt(2). The `identity_dist_matrix` function performs this
#' calculation for every pairwise rows in a matrix.
#' @param x a numeric vector
#' @param y a numeric vector
#' @param iqr logical, default to FALSE. If set to TRUE, then IQR of the
#' identity distance is returned.
#' @export
#' @rdname identity_distance
#' @examples
#' set.seed(1)
#' x = rnorm(100)
#' identity_dist(x = x, y = 3*x, iqr = FALSE)
#' identity_dist(x = x, y = 3*x, iqr = TRUE)
identity_dist = function(x, y, iqr = FALSE){
  if(any(is.na(x))|any(is.na(y))){
    warning("NA exists in either x or y, identity distance calculation removes these")
  }
  d = (x - y)/sqrt(2)
  id = stats::median(abs(d), na.rm = TRUE)
  if(iqr) {
    result = c("id" = id,
               "iqr_d" = unname(stats::quantile(d, 0.75, na.rm = TRUE) -
                                  stats::quantile(d, 0.25, na.rm = TRUE)))
    return(result)
  } else {
    return(id)
  }
}

#' @title Calculate identity distance for a matrix
#' @param matrix a matrix
#' @importFrom proxy dist
#' @rdname identity_distance
#' @export
#' @examples
#' set.seed(1)
#' x = matrix(rnorm(100), ncol = 20)
#' identity_dist_matrix(x)
identity_dist_matrix = function(matrix){
  proxy::dist(matrix, method = identity_dist)
}

#' Coefficient of repeatability
#' @param x a vector
#' @param y a vector
#' @export
#' @examples
#' set.seed(1)
#' x = rnorm(100)
#' y = rnorm(100)
#' # identity_dist(x, y)
#' coef_repeat(x, y)
coef_repeat = function(x, y){
  d = x - y
  sd = sqrt(mean(d^2))
  result = c("mean_d" = mean(d),
             "sd_d" = sd)
  return(result)
}

#' @rdname identity_distance
#' @export
calcIdenDist = function(matrix){
  .Deprecated(new = "identity_dist_matrix")
  proxy::dist(matrix, method = identity_dist)
}

#' @rdname identity_distance
#' @export
identityDist = function(x, y, iqr = FALSE){
  .Deprecated(new = "identity_dist")
  d = (x - y)/sqrt(2)
  id = stats::median(abs(d))
  if(iqr) {
    result = c("id" = id,
               "iqr_d" = unname(stats::quantile(d, 0.75) -
                                  stats::quantile(d, 0.25)))
    return(result)
  } else {
    return(id)
  }
}
