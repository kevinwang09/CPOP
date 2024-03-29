% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/penalties.R
\name{colmeans_penalty}
\alias{colmeans_penalty}
\alias{p_transform}
\alias{tstat_penalty}
\alias{ks_penalty}
\title{Compute feature-wise absolute difference penalty}
\usage{
colmeans_penalty(x1, x2)

p_transform(penalty)

tstat_penalty(x1, x2, y1, y2)

ks_penalty(x1, x2, statistic = TRUE)
}
\arguments{
\item{x1}{A data matrix}

\item{x2}{A data matrix}

\item{penalty}{A vector of penalties to be transformed
where the sum is set to the length of the vector}

\item{y1}{A factor response corresponding to the columns of x1}

\item{y2}{A factor response corresponding to the columns of x2}

\item{statistic}{Should the KS-test statistic be returned (default) or the p-value should be
returned. Logical.}
}
\value{
A vector

A vector

A vector of length matching that of x1 and x2
}
\description{
Compute feature-wise absolute difference penalty

Compute t-stat absolute difference penalty
}
\examples{
n = 20
p = 5
x1 = matrix(rnorm(n * p, mean = 0, sd = 1), nrow = n, ncol = p)
x2 = matrix(rnorm(n * p, mean = 1, sd = 1), nrow = n, ncol = p)
colmeans_penalty(x1, x2)
n = 20
p = 5
x1 = matrix(rnorm(n * p, mean = 0, sd = 1), nrow = n, ncol = p)
y1 = factor(rbinom(n, 1, prob = 0.5), levels = c("0", "1"))
x2 = matrix(rnorm(n * p, mean = 1, sd = 1), nrow = n, ncol = p)
y2 = factor(rbinom(n, 1, prob = 0.5), levels = c("0", "1"))
tstat_penalty(x1 = x1, x2 = x2, y1 = y1, y2 = y2)
n = 20
p = 5
x1 = matrix(rnorm(n * p, mean = 0, sd = 1), nrow = n, ncol = p)
y1 = factor(rbinom(n, 1, prob = 0.5), levels = c("0", "1"))
x2 = matrix(rnorm(n * p, mean = 1, sd = 1), nrow = n, ncol = p)
y2 = factor(rbinom(n, 1, prob = 0.5), levels = c("0", "1"))
ks_penalty(x1 = x1, x2 = x2)
}
