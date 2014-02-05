#' Produce a data frame containing a grid.
#' @examples
#' str(reduce_grid(diamonds[1:3], bins = 4))
#' str(reduce_grid(diamonds[1:3], bins = 2ci))
#' @export
reduce_grid <- function(data, bins = 5, breaks = list()) {
  breaks <- default_breaks(data, bins, breaks)
  datagrid <- expand.grid(breaks, KEEP.OUT.ATTRS = FALSE)
  factorgrid <- datagrid

  list(
    factorgrid=factorgrid,
    datagrid=datagrid
  )
}

# Function to estimate the number of rows that the reduce function will create
# You'd use this to warn the user if they're going to create something that's
# way too big.
reduce_grid_n <- function(data, ...) {
  lengths <- sapply(default_breaks(data, ...), "length")
  prod(lengths)
}

#' Create default breaks for those the user didn't specify
#' @examples
#' default_breaks(diamonds)
#' default_breaks(diamonds, bins = 4)
#' @export
default_breaks <- function(data, bins = 5, breaks = list()) {
  missing_breaks <- setdiff(names(data), names(breaks))
  for(var in missing_breaks) {
    breaks[[var]] <- var_breaks(data[[var]], bins)
  }

  breaks
}

#' Determins if the variable is of factor type and if not,
#' create the break values.
#' @examples
#' var_breaks(diamonds)
#' @export
var_breaks <- function(x, bins) {
  if (is.factor(x)) {
    levels(x)
  } else {
    unname(breaks(x, "n", bins))
  }
}
