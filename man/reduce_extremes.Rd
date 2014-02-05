\name{reduce_extremes}
\alias{reduce_extremes}
\title{Subset the dataframe by collecting records that have either the maximum...}
\usage{reduce_extremes(data, ...)}
\description{Subset the dataframe by collecting records that have either the maximum 
or minimum for any variable
In an effort to lower the number of plots that need to be created by the 
LinVal package when using a highly complex data source, the records that 
contain either the maximum or minimum value for any of the variables are 
collected for plotting.}
\author{Christopher Kielion \email{ckielion@gmail.com}}
\arguments{\item{datagrid}{dataframe from which to determine maximums/minimums and 
collect records that contain these values}}
\examples{selected_data <- diamonds[,c('carat','cut','depth','table')]

print(str(selected_data)) 
# Notice that the data types of the variables are as they appear
# in the source dataframe

output <- reduce_extremes(selected_data)
print(output$factorgrid) 
# Notice that the observations have been subset to only include those
# that contain a maximum or minimum value for any numeric varible in 
# the frame.

print(str(output$datagrid))   
# This shows that the output contains the dataframe in its original
# form as well so that predicted values can be made and appended to the
# factorgrid produced.}
