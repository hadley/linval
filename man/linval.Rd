\name{linval}
\alias{linval}
\title{Breaks up the data into groups according to the user specified type and
then plots using the geoms found in the ggplot2 package.}
\usage{
linval(formula, data, reduction, ...)
}
\arguments{
  \item{formula}{formula should be expressed as the model
  of interest in the form y ~ x1 + x2 + ...}

  \item{data}{input data}

  \item{reduction}{Either a string or a function. If a
  string, looks for function called \code{paste("reduce_",
  reduction)}. Used to reduce the size of the data to make
  visualisation easier.}

  \item{...}{Other parameters passed on to reduction
  function.}
}
\description{
Breaks up the data into groups according to the user
specified type and then plots using the geoms found in the
ggplot2 package.
}
\examples{
# By quantile
d_quantiles <- linval(price ~ carat + color + clarity, data = diamonds,
  reduction = "qgrid", bins = 4)

d_evenly_spaced <- linval(formula = price ~ carat + color + clarity,
  data = diamonds, reduction = "egrid")

d_extremes <- linval(price ~ depth + table + carat + color + clarity,
  data = diamonds, reduction = "extremes")

d_user_breaks <- linval(price ~ carat + color + clarity,
  data = diamonds, reduction = "egrid",
  breaks = list(carat = c(0.4, 0.8, 3.0)))

d_as_is <- linval(price ~ carat + color + clarity,
  data = diamonds, reduction = "nothing")
}

