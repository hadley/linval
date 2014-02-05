\name{plot.linval}
\alias{plot.linval}
\title{Plots a previously created linval object}
\usage{
\method{plot}{linval}(x, geom = NULL, hierarchy = NULL,
  frame = "plotgrid")
}
\arguments{
  \item{x}{x is this is the name of the previously created
  linval object.}

  \item{geom}{geom is the name of the qplot plot type that
  should be used from the ggplot2 package.}

  \item{hierarchy}{hierarchy sets the facetting
  configuration for plotting. Only the x variables in the
  model will be used in the hierarchy and the first x
  variable in the x portion of the hierarchy will be used
  on the horizontal axis for plotting.}

  \item{frame}{frame is the name of the frame saved in the
  linval object that you would like plotted.  The default
  is the plotgrid which is the primary purpose of the
  package but the dataframe and the factor grid can also be
  plotted for diagnostic purposes.}
}
\description{
Plots a previously created linval object
}
\examples{
plot.linval(x = breaks_by_user_specs, geom = 'point',
  hierarchy = carat ~ color + clarity)
}
\author{
Christopher Kielion \email{ckielion@gmail.com}
}

