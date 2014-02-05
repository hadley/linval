\name{breaktranslations}
\alias{breaktranslations}
\title{Translate the percentiles into their equivalent data bins...}
\usage{breaktranslations(datagrid, nperc)}
\description{Translate the percentiles into their equivalent data bins
This function finds its primary use within the breaksandpercents 
capability of the linval package.  BreaksandPercents is set up in a way 
that it minimizes the amount of code that is required to group either 
prespecified Breaks or the Quantile breaks in the data. If a breaks 
parameter is not passed into the linval function and nperc is specified
(along with the stated factoring structure) breaktranslations will figure 
out which values within each variable make up the quantiles specified by 
nperc.  This then builds the breaks that represent these percentage 
quantiles and returns them as if the user had specified the breaks.}
\author{Christopher Kielion \email{ckielion@gmail.com}}
\arguments{\item{datagrid}{dataframe from which to derive bins}
\item{nperc}{number that divides 1 to get percentiles}}
\examples{selected_data <- diamonds[,c('carat','cut','depth','table')]

print(str(selected_data)) 
# Notice that the data types of the variables are as they appear
# in the source dataframe

output <- breaktranslations(selected_data, nperc = 4)
output_comparison <- sapply(diamonds, quantile,  prob = seq(0,1,1/4))

# output_comparison contains the cut offs for 25%, 50%, 75%, and 100% of 
# the data percentiles

print(output_comparison) 
# This is only printed for reference

print(output)            
# This shows that the breaks were created based on the nperc input
# parameter and that the values match the quantile output for the
# numeric variables only.}
