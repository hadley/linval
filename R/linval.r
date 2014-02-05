#' Breaks up the data into groups according to the user specified type and 
#' then plots using the geoms found in the ggplot2 package.
#'
#' @param formula formula should be expressed as the model of interest in the 
#' form y ~ x1 + x2 + ...
#' @param data data is the name of the dataframe where the variables in the 
#' formula are found.
#' @param reduction reduction describes how the data should be broken up to
#' make plotting and visualization easier.
#' @param breaks breaks is set only if the "breakslg" reduction is chosen.  
#' These are the user specified breaks and should be set for ALL x variables 
#' in the hierarchy. 
#' @param bins bins is set only if user would like to use the default equally
#' sized bins or if they would like the breaks of numeric variables 
#' determined by percentiles. It's value divides the values into percentiles
#' equal to 1/bins. Default=5.
#' @author Christopher Kielion \email{ckielion@@gmail.com}
#' @examples
#' data(diamonds)
#' 
#' breaks_by_quantile_data <- linval(
#'   formula = price ~ carat + color + clarity, data = diamonds, 
#'   reduction="breaks", bins=4)
#'
#' hi_and_low_vals <- linval(
#'   formula = price ~ depth + table + carat + color + clarity, 
#'   data = diamonds, reduction="extremes")
#' 
#' breaks_by_user_specs <- linval(formula = price ~ carat + color + clarity, 
#'   data = diamonds, reduction="breaks", breaks=list(carat=c(0.4, 0.8, 3.0)))
#' 
#' data_factorized_asis <- linval(formula = price ~ carat + color + clarity, 
#'   data = diamonds, reduction="nothing")
#' 
#' data_gridded <- linval(formula = price ~ carat + color + clarity,
#'   data = diamonds, reduction="grid")
linval <- function(formula, data, reduction=NULL, breaks=NULL, bins=NULL){

  ## this makes sure that the plot type that is input into the program is an option in the first place.
  reduction <- match.arg(reduction, 
    c("breaks", "extremes", "nothing", "grid"))
  
  ## This converts the formula from the equation that describes the model to actually fitting it.
  model <- lm(formula, data)

  # Since the datagrid will be used to predict the value for the response
  # variable, it is not included mainly for organization purposes 
  xs <- all.vars(terms(formula)[[3]])
  yy <- all.vars(terms(formula)[[2]])
  data <- data[, xs] 
  
  ## Perform the grid interpolation
  interpolator <- match.fun(paste('reduce', reduction, sep = '_'))
  interp <- interpolator(data = data, breaks = breaks, bins = bins)  

  pred <- predict(model, interp$datagrid, se = TRUE)[c("fit", "se.fit")]
  
  interp$datagrid <- cbind(interp$datagrid, pred)
  interp$factorgrid <- cbind(interp$factorgrid, pred)
  interp$plotgrid<-cast(interp$factorgrid, formula = formula(paste(paste(xs, sep=' ', collapse ='+'), '~ .')), mean, value = 'fit')
  names(interp$plotgrid)<- c(xs,'fit')
  interp$formula <- formula

  structure(interp, class = "linval")
}

#' Plots a previously created linval object
#'
#' @param x x is this is the name of the previously created linval object.
#' @param geom geom is the name of the qplot plot type that should be used 
#' from the ggplot2 package.
#' @param hierarchy hierarchy sets the facetting configuration for plotting.  
#' Only the x variables in the model will be used in the hierarchy and the 
#' first x variable in the x portion of the hierarchy will be used on the 
#' horizontal axis for plotting.
#' @param frame frame is the name of the frame saved in the linval object
#' that you would like plotted.  The default is the plotgrid which is the
#' primary purpose of the package but the dataframe and the factor grid
#' can also be plotted for diagnostic purposes.   
#' @author Christopher Kielion \email{ckielion@@gmail.com}
#' @examples
#'
#' plot.linval(x = breaks_by_user_specs, geom = 'point',
#'   hierarchy = carat ~ color + clarity) 
plot.linval <- function(x, geom=NULL, hierarchy = NULL, frame = 'plotgrid') {
  formula <- x$formula
  data <- x$datagrid
  
  xs <- all.vars(terms(formula)[[3]])
  ys <- all.vars(terms(formula)[[2]])
  if (sum(xs %in% all.vars(hierarchy)) < length(xs)) 
    stop('Model and Hierarchy Disagree')

  ## Checks for missings in the data only within the variables present in the model. Returns
  ## TRUE if there are missings and FALSE if not.
  if(max(is.na(data[xs]))>0) missings <- TRUE else missings <- FALSE

  ## Within the hierarchy, the first X variable is used on the major horizontal axis, therefore, it 
  ## must be removed so that it is not reused in the faceting of plots.
  hierarchyx <- all.vars(terms(hierarchy)[[3]])[1]
  if (length(all.vars(terms(hierarchy)[[3]])) == 1) {
    remxterms <- "."
  } else {
    remxterms <- all.vars(terms(hierarchy)[[3]])[-1]
  }
  remhierarchy <- as.formula(
    paste(
      paste(all.vars(terms(hierarchy)[[2]]),collapse = '+'),
      " ~ ",
      paste(remxterms,  collapse= "+")
    )
  )
  
  ggplot(x[[frame]], aes_string(x = hierarchyx, y = "fit")) + 
    layer(geom = geom) +
    facet_grid(remhierarchy) + 
    xlab(hierarchyx) + ylab(ys)
}
