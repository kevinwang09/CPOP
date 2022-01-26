#' A better pairs plot function
#'
#' @param x a vector
#' @param y a vector
#' @author Kevin Wang
#' @rdname additional_panels
#' @export
#' @examples
#' pairs(iris[,-5], lower.panel = panel_cor)
panel_cor <- function(x, y)
{
  usr <- graphics::par("usr"); on.exit(graphics::par(usr))
  graphics::par(usr = c(0, 1, 0, 1))
  r <- stats::cor(x, y, method = "pearson")
  txt <- format(c(r, 0.123456789), digits = 3)[1]
  graphics::text(0.5, 0.5, paste("",txt), cex = 2.5)
}



#' @title pairs panel with identity distance
#' @param x a vector
#' @param y a vector
#' @author Kevin Wang
#' @rdname additional_panels
#' @export
#' @examples
#' pairs(iris[,-5],
#' upper.panel = panel_scatter_abline,
#' lower.panel = panel_idenDist)
panel_idenDist <- function(x, y)
{
  usr <- graphics::par("usr"); on.exit(graphics::par(usr))
  graphics::par(usr = c(0, 1, 0, 1))
  cor <- stats::cor(x, y, method = "pearson")
  corTxt <- format(c(cor, 0.123456789), digits = 3)[1]
  idenDist <- identity_dist(x, y)
  idenDistTxt <- format(c(idenDist, 0.123456789), digits = 3)[1]
  graphics::text(0.5, 0.5, paste0("Corr: ", corTxt, "\n iden-dist: ", idenDistTxt), cex = 2.5)
}

#' @title A better pairs plot function
#' @description scatter plot panel with the identity line
#' @param x a vector
#' @param y a vector
#' @param a a for abline
#' @param b b for abline
#' @param col col for abline
#' @author Kevin Wang
#' @rdname additional_panels
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
#' @rdname additional_panels
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
#' @rdname additional_panels
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
#' @rdname additional_panels
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
