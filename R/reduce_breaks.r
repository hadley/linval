#' Databreak divides the data elements into groups depending on the type that
#' they elect to use. Specifying breaks will allow the user to set the breaks
#' by hand for all variables.  These can be thought of as the bins of the
#' data that the user will specify.  However, if bins is supplied and breaks
#' is not, then quantiles will be derived as (1/bins)*100  of the data.
#' Think of bins as saying that you want nperc roughly even sized groups.
#'
#' @param datagrid datagrid is the name of the dataframe that should be used
#' to develop the factorgrid and is also used as the source of values to
#' predict from.  It contains only those variables that are in the formula
#' for the proposed model.
#' @param breaks breaks is set when the user would like to specify the data
#' breaks for x variables in the model.
#' @param bins bins is used when breaks are not set for all numeric variables
#' or if it is desired that the variables be divided by percentiles equal to
#' 1/bins.
#' @author Christopher Kielion \email{ckielion@@gmail.com}
#' @export
#' @examples
#' selected_data <- diamonds[,c('carat','cut','depth','table')]
#'
#' str(selected_data)
#' # Notice the data types of the variables
#'
#' break_output <- reduce_breaks(selected_data,
#    breaks =list(carat=c(0.4, 0.8, 1), depth=c(53, 61, 61.5, 63, 75),
#      table=c(43, 57, 95)))
#' print(str(break_output$factorgrid))
#' # This output shows that the variable values were divided given the
#' # user specified numeric cutoffs
#'
#' percent_output <- reduce_breaks(selected_data, bins=3)
#' str(percent_output$factorgrid)
#' # This output shows that the variable values were separated into
#' # groups as specified by quantiles as expressed by 1/bins
#'
#' str(break_output$datagrid)
#' # This shows that the output contains the dataframe in its original
#' # form as well so that predicted values can be made and appended to the
#' # factorgrid in the ouput for graphics generation when breaks are given.
#'
#' str(percent_output$datagrid)
#' # This shows that the output contains the dataframe in its original
#' # form as well so that predicted values can be made and appended to the
#' # factorgrid in the ouput for graphics generation when bins is given.
reduce_breaks<-function(data, breaks=NULL, bins=NULL, ...){
  cont_cols <- sapply(data, is.numeric)
  cont_df <- data[cont_cols]

  breaks <- default_breaks(cont_df, breaks = breaks)

  for (i in names(breaks)) {
    breaks[[i]] <- sort(as.numeric(unique(matrix(c(breaks[[i]], max(data[[i]]), min(data[[i]]))))))
  }

  cutter <- function(var_name) {
    cut(cont_df[[var_name]], breaks = breaks[[var_name]], include.lowest = TRUE)
  }
  cut_vars <- lapply(names(breaks), cutter)
  names(cut_vars) <- names(breaks)

  factorgrid <- data.frame(cut_vars, data[!cont_cols])

  list(
    factorgrid = factorgrid,
    datagrid = data
  )
}
