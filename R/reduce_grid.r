#' Reduce a dataset into a grid.
#'
#' \code{reduce_egrid} makes a grid with equal width bins, \code{reduce_qgrid}
#' makes a grid with bins containing equal numbers of points,
#' \code{reduce_extremes} captures just the minimum and maximum of each
#' variable.
#'
#' @param data input data
#' @param bin number of bins for each variable
#' @param breaks a named list. Use this to supply custom breaks for
#'   individual variables
#' @param n_max Maximum number of rows to create (used to product against
#'   accidentally creating excessively large outputs).
#' @examples
#' reduce_egrid(diamonds[1:3], bins = 4)
#' reduce_qgrid(diamonds[1:3], bins = 4)
#' reduce_extremes(diamonds[1:3])
#' @export
reduce_egrid <- function(data, bins = 5, breaks = list(), n_max = 1e5) {
  build_grid(data, function(x) seq_range(x, 5), breaks, n_max)
}

#' @rdname reduce_egrid
#' @export
reduce_qgrid <- function(data, bins = 5, breaks = list(), n_max = 1e5) {
  build_grid(data, function(x) quantile(x, probs), breaks, n_max)
}

#' @rdname reduce_egrid
#' @export
reduce_extremes <- function(data, breaks = list(), n_max = 1e5) {
  build_grid(data, function(x) range(x, na.rm = TRUE), breaks, n_max)
}

seq_range <- function(x, n) {
  rng <- range(x, na.rm = TRUE)
  seq(rng[1], rng[2], length = n)
}

build_grid <- function(data, break_fun, default_breaks, n_max = 1e5) {
  is_numeric <- vapply(data, is.numeric, logical(1))

  breaks <- c(
    lapply(data[is_numeric], break_fun),
    lapply(data[!is_numeric], unique2)
  )
  breaks <- modifyList(breaks, default_breaks)

  # Check it's not going to be too big!
  n <- prod(vapply(breaks, length, integer(1)))
  if (n > n_max) {
    stop("Would create ", n, " rows. Increase n_max to proceed",
      call. = FALSE)
  }

  structure(list(breaks = breaks), class = "grid")
}

#' @export
print.grid <- function(x, ...) {
  cat("<grid>\n")
  labels <- vapply(x$breaks, paste, collapse = ", ", FUN.VALUE = character(1))
  bullets <- paste0("* ", names(x$breaks), ": ", labels, collapse = "\n")
  cat(bullets, "\n", sep = "")
}

#' @export
as.data.frame.grid <- function(x, ...) {
  expand.grid(x$breaks, KEEP.OUT.ATTRS = FALSE)
}

# Why isn't unique implemented like this already??
unique2 <- function(x) {
  if (is.ordered(x)) {
    ordered(levels(x))
  } else if (is.factor(x)) {
    factor(levels(x))
  } else {
    unique(x)
  }
}
