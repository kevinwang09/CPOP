% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/distances.R
\name{coef_repeat}
\alias{coef_repeat}
\title{Coefficient of repeatability}
\usage{
coef_repeat(x, y)
}
\arguments{
\item{x}{a vector}

\item{y}{a vector}
}
\description{
Coefficient of repeatability
}
\examples{
set.seed(1)
x = rnorm(100)
y = rnorm(100)
# identity_dist(x, y)
coef_repeat(x, y)
}
