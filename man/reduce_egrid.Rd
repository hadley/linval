\name{reduce_egrid}
\alias{reduce_egrid}
\alias{reduce_extremes}
\alias{reduce_qgrid}
\title{Reduce a dataset into a grid.}
\usage{
reduce_egrid(data, bins = 5, breaks = list(), n_max = 1e+05)

reduce_qgrid(data, bins = 5, breaks = list(), n_max = 1e+05)

reduce_extremes(data, breaks = list(), n_max = 1e+05)
}
\arguments{
  \item{data}{input data}

  \item{bin}{number of bins for each variable}

  \item{breaks}{a named list. Use this to supply custom
  breaks for individual variables}

  \item{n_max}{Maximum number of rows to create (used to
  product against accidentally creating excessively large
  outputs).}
}
\description{
\code{reduce_egrid} makes a grid with equal width bins,
\code{reduce_qgrid} makes a grid with bins containing equal
numbers of points, \code{reduce_extremes} captures just the
minimum and maximum of each variable.
}
\examples{
reduce_egrid(diamonds[1:3], bins = 4)
reduce_qgrid(diamonds[1:3], bins = 4)
reduce_extremes(diamonds[1:3])
}

