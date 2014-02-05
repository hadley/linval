\name{linval}
\alias{linval}
\title{Breaks up the data into groups according to the user specified type and...}
\usage{linval(formula, data, reduction, breaks, bins)}
\description{Breaks up the data into groups according to the user specified type and 
then plots using the geoms found in the ggplot2 package.}
\author{Christopher Kielion \email{ckielion@gmail.com}}
\arguments{\item{formula}{formula should be expressed as the model of interest in the 
form y ~ x1 + x2 + ...}
\item{data}{data is the name of the dataframe where the variables in the 
formula are found.}
\item{reduction}{reduction describes how the data should be broken up to
make plotting and visualization easier.}
\item{breaks}{breaks is set only if the "breakslg" reduction is chosen.  
These are the user specified breaks and should be set for ALL x variables 
in the hierarchy.}
\item{bins}{bins is set only if user would like to use the default equally
sized bins or if they would like the breaks of numeric variables 
determined by percentiles. It's value divides the values into percentiles
equal to 1/bins. Default=5.}}
\examples{data(diamonds)

breaks_by_quantile_data <- linval(
formula = price ~ carat + color + clarity, data = diamonds, 
reduction="breaks", bins=4)

hi_and_low_vals <- linval(
formula = price ~ depth + table + carat + color + clarity, 
data = diamonds, reduction="extremes")

breaks_by_user_specs <- linval(formula = price ~ carat + color + clarity, 
data = diamonds, reduction="breaks", breaks=list(carat=c(0.4, 0.8, 3.0)))

data_factorized_asis <- linval(formula = price ~ carat + color + clarity, 
data = diamonds, reduction="nothing")

data_gridded <- linval(formula = price ~ carat + color + clarity,
data = diamonds, reduction="grid")}
