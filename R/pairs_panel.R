#' A better pairs plot function
#'
#' @param x a vector
#' @param y a vector
#' @author Kevin Wang
#' @export
#' @examples
#' pairs(iris[,-5], lower.panel = panel_cor)
panel_cor <- function(x, y)
{
  usr <- graphics::par("usr"); on.exit(graphics::par(usr))
  graphics::par(usr = c(0, 1, 0, 1))
  # r <- cor(x[subset], y[subset], method ="pearson")
  r <- stats::cor(x, y, method ="pearson")
  txt <- format(c(r, 0.123456789), digits = 3)[1]
  # if(all(subset)){
  graphics::text(0.5, 0.25, paste("",txt), cex = 2)
  # } else {
  #   text(0.5, 0.25, paste("subset Corr=",txt), cex = 2)
  # }

}

#' Identity Distance
#' @param x a vector
#' @param y a vector
#' @export
#' @examples
#' set.seed(1)
#' x = rnorm(100)
#' identityDist(x = x, y = x)
#' identityDist(x = x, y = 2*x)
identityDist = function(x, y){
  res = stats::median(abs(x - y)/sqrt(2))
  return(res)
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

#' @title pairs panel with identity distance
#' @param x a vector
#' @param y a vector
#' @author Kevin Wang
#' @export
#' @examples
#' pairs(iris[,-5],
#' upper.panel = panel_scatter_abline,
#' lower.panel = panel_idenDist)
panel_idenDist <- function(x, y)
{
  usr <- graphics::par("usr"); on.exit(graphics::par(usr))
  graphics::par(usr = c(0, 1, 0, 1))
  cor <- stats::cor(x, y, method ="pearson")
  corTxt <- format(c(cor, 0.123456789), digits = 3)[1]
  idenDist <- identityDist(x, y)
  idenDistTxt <- format(c(idenDist, 0.123456789), digits = 3)[1]


  graphics::text(0.5, 0.25, paste("", corTxt, "\n (", idenDistTxt, ")"), cex = 2)
}

#' @title A better pairs plot function
#' @description scatter plot panel with the identity line
#' @param x a vector
#' @param y a vector
#' @param a a for abline
#' @param b b for abline
#' @param col col for abline
#' @author Kevin Wang
#' @export
#' @examples
#' pairs(iris[,-5],
#' lower.panel = panel_cor,
#' upper.panel = panel_scatter_abline)

panel_scatter_abline <- function(x, y, a = 0, b = 1, col = "red")
{
  graphics::points(x, y)
  graphics::abline(a = a, b = b, col = col)
}

#' @title A better pairs plot function
#' @description panel_scatter_abline with small dots
#' @param x a vector
#' @param y a vector
#' @param a a for abline
#' @param b b for abline
#' @param col col for abline
#' @author Kevin Wang
#' @export
#' @examples
#' pairs(iris[,-5],
#' lower.panel = panel_cor,
#' upper.panel = panel_scatter_abline_smallDots)
panel_scatter_abline_smallDots <- function(x, y, a = 0, b = 1, col = "red")
{
  graphics::points(x, y, pch = ".")
  graphics::abline(a = a, b = b, col = col)
}

#' @title A better pairs plot function
#' @description panel_scatter_abline with h and v lines at zero
#' @param x a vector
#' @param y a vector
#' @author Kevin Wang
#' @export
#' @examples
#' pairs(iris[,-5],
#' lower.panel = panel_cor,
#' upper.panel = panel_scatter_abhvline)
panel_scatter_abhvline = function(x, y)
{
  graphics::points(x, y)
  graphics::abline(a = 0, b = 1, col = "red")
  graphics::abline(h = 0, col = "blue")
  graphics::abline(v = 0, col = "blue")
}


#' @title A better pairs plot function
#' @description panel_scatter_smallDots with h and v lines at zero
#' @param x a vector
#' @param y a vector
#' @author Kevin Wang
#' @export
#' @examples
#' pairs(iris[,-5],
#' lower.panel = panel_cor,
#' upper.panel = panel_scatter_smallDots_abhvline)
panel_scatter_smallDots_abhvline = function(x, y)
{
  graphics::points(x, y, pch = ".")
  graphics::abline(a = 0, b = 1, col = "red")
  graphics::abline(h = 0, col = "blue")
  graphics::abline(v = 0, col = "blue")
}
