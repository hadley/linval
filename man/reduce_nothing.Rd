\name{reduce_nothing}
\alias{reduce_nothing}
\title{Convert a dataframe into a dataframe whose variables are of Factor type...}
\usage{reduce_nothing(data, ...)}
\description{Convert a dataframe into a dataframe whose variables are of Factor type
The direct factors function converts the dataframe of interest into a frame
of Factor type variables.  In an effort to reduce the number of factors 
especially in the event of continuous variables, the values may be rounded 
to 2 decimal places. The end result of this function is a dataframe with 
the data as it was read into the function as well as a frame of the 
variables in Factor form.  None of the observations in either of the frames 
are reordered so that when the datagrid is used to predict the response 
value, the predicted column can be simply column bound to the factorized 
data frame for immediate plotting.}
\author{Christopher Kielion \email{ckielion@gmail.com}}
\arguments{\item{datagrid}{data frame that needs to be converted into a factor frame}}
\examples{selected_data <- diamonds[,c('carat','cut','depth','table')] 

str(selected_data) 
# Notice the data types of the variables

output <- reduce_nothing(selected_data)
str(output$factorgrid) 
# Now, notice that the numeric variables have been rounded and that all 
# variables are of type factor when in the beginning, some were not.

str(output$datagrid)   
# This shows that the output contains the dataframe in its original form as 
# well so that predicted values can be made and appended to the factorgrid 
# in the ouput for graphics generation.}
