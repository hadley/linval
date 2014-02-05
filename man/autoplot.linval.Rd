\name{autoplot.linval}
\alias{autoplot.linval}
\title{Plots a previously created linval object}
\usage{
\method{autoplot}{linval}(x, hierarchy = . ~ ., geom = NULL)
}
\arguments{
  \item{x}{is this is the name of the previously created
  linval object.}

  \item{geom}{geom to use. Guess automatically based on
  whether the predictor is on the y-axis (the default), or
  elsewhere.}

  \item{hierarchy}{override the ordering of variables from
  the model.  Combined with the original formula using
  \code{\link{update.formula}} so you use can \code{.} as a
  place holder.}
}
\description{
Plots a previously created linval object
}
\examples{
d_quantiles <- linval(price ~ carat + color + clarity, data = diamonds,
  reduction = "qgrid", bins = 4)

autoplot(d_quantiles)

# Use the second argument to rearrange the order of variables
autoplot(d_quantiles, . ~ color + clarity + carat)
autoplot(d_quantiles, color ~ clarity + price + .)
}

