\name{reduce_grid}
\alias{reduce_grid}
\title{Produce a data frame containing a grid.}
\usage{
reduce_grid(data, bins = 5, breaks = list())
}
\description{
Produce a data frame containing a grid.
}
\examples{
str(reduce_grid(diamonds[1:3], bins = 4))
str(reduce_grid(diamonds[1:3], bins = 2ci))
}

