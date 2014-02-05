#' Subset the dataframe by collecting records that have either the maximum 
#' or minimum for any variable
#' In an effort to lower the number of plots that need to be created by the 
#' LinVal package when using a highly complex data source, the records that 
#' contain either the maximum or minimum value for any of the variables are 
#' collected for plotting.
#'
#' @param datagrid dataframe from which to determine maximums/minimums and 
#' collect records that contain these values
#' @author Christopher Kielion \email{ckielion@@gmail.com}
#' @examples
#' selected_data <- diamonds[,c('carat','cut','depth','table')]
#'
#' print(str(selected_data)) 
#' # Notice that the data types of the variables are as they appear
#' # in the source dataframe
#'
#' output <- reduce_extremes(selected_data)
#' print(output$factorgrid) 
#' # Notice that the observations have been subset to only include those
#' # that contain a maximum or minimum value for any numeric varible in 
#' # the frame.
#'                                
#' print(str(output$datagrid))   
#' # This shows that the output contains the dataframe in its original
#' # form as well so that predicted values can be made and appended to the
#' # factorgrid produced.


reduce_extremes<-function(data, ...){
    codedxs <- sapply(data, is.numeric)
    xsnum <- names(codedxs[codedxs == TRUE])

    hilowvals <- as.data.frame(matrix(c(apply(data[xsnum], MARGIN=2, FUN=min, na.rm=TRUE),
      apply(data[xsnum], MARGIN=2, FUN=max, na.rm=TRUE)), nrow=2, ncol=length(xsnum), byrow=TRUE))
    names(hilowvals) <- xsnum

    hilowcheck <- function(checkvar, data, hilowvals) {
      data[, checkvar] %in% hilowvals[, checkvar]
    }
    a <- as.data.frame(lapply(xsnum, hilowcheck, data=data, hilowvals=hilowvals))
    names(a) <- xsnum
    b <- as.data.frame(apply(a, MARGIN=1, FUN=sum))[[1]]
    factorgrid <- as.data.frame(apply(data, MARGIN=2, FUN=as.factor))
    factorgrid<-factorgrid[b>=1,]
    data<-data[b>=1,]

    list(
      datagrid = data,
      factorgrid = factorgrid
    )
}
