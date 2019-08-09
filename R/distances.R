#' Identity distance between two vectors
#' @param x a vector
#' @param y a vector
#' @param iqr logical
#' @export
#' @examples
#' set.seed(1)
#' x = rnorm(100)
#' identityDist(x = x, y = 3*x, iqr = FALSE)
#' identityDist(x = x, y = 3*x, iqr = TRUE)
identityDist = function(x, y, iqr = FALSE){
  d = (x - y)/sqrt(2)
  id = stats::median(abs(d))
  if(iqr) {
    result = c("id" = id,
               "iqr_d" = unname(quantile(d, 0.75) - quantile(d, 0.25)))
    return(result)
  } else {
    return(id)
  }
}

#' Identity Distance for a matrix
#' @param matrix a matrix
#' @export
#' @importFrom proxy dist
#' @examples
#' set.seed(1)
#' x = matrix(rnorm(100), ncol = 10)
#' calcIdenDist(x)
calcIdenDist = function(matrix){
  proxy::dist(matrix, method = identityDist)
}



#' Coefficient of repeatability
#' @param x a vector
#' @param y a vector
#' @export
#' @examples
#' set.seed(1)
#' x = rnorm(100)
#' y = rnorm(100)
#' # identityDist(x, y)
#' coef_repeat(x, y)
coef_repeat = function(x, y){
  d = x - y
  sd = sqrt(mean(d^2))
  result = c("mean_d" = mean(d),
             "sd_d" = sd)
  return(result)
}
