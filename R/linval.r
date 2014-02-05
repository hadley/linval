#' Breaks up the data into groups according to the user specified type and
#' then plots using the geoms found in the ggplot2 package.
#'
#' @param formula formula should be expressed as the model of interest in the
#'   form y ~ x1 + x2 + ...
#' @param data input data
#' @param reduction Either a string or a function. If a string, looks for
#'    function called \code{paste("reduce_", reduction)}. Used to reduce
#'    the size of the data to make visualisation easier.
#' @param ... Other parameters passed on to reduction function.
#' @export
#' @examples
#' # By quantile
#' d_quantiles <- linval(price ~ carat + color + clarity, data = diamonds,
#'   reduction = "qgrid", bins = 4)
#'
#' d_evenly_spaced <- linval(formula = price ~ carat + color + clarity,
#'   data = diamonds, reduction = "egrid")
#'
#' d_extremes <- linval(price ~ depth + table + carat + color + clarity,
#'   data = diamonds, reduction = "extremes")
#'
#' d_user_breaks <- linval(price ~ carat + color + clarity,
#'   data = diamonds, reduction = "egrid",
#'   breaks = list(carat = c(0.4, 0.8, 3.0)))
#'
#' d_as_is <- linval(price ~ carat + color + clarity,
#'   data = diamonds, reduction = "nothing")
linval <- function(formula, data, reduction, ...){
  # Fit the model
  model <- lm(formula, data, model = FALSE)

  # Extract predictors from original data
  xs <- all.vars(terms(formula)[[3]])
  in_model <- data[xs]

  # Generate reduced grid for predictions
  if (is.character(reduction)) {
    fname <- paste0("reduce_", reduction)
    reduction <- match.fun(fname)
  }
  stopifnot(is.function(reduction))

  grid <- as.data.frame(reduction(in_model, ...))

  y <- all.vars(terms(formula)[[2]])
  pred <- predict(model, newdata = grid, se = TRUE)
  pred_df <- data.frame(pred[c("fit", "se.fit")])
  names(pred_df) <- paste0(y, c("", ".se"))
  grid <- cbind(grid, pred_df)

  structure(list(
    formula = formula, grid = grid, model = model, xs = xs, y = y),
    class = "linval")
}

#' Plots a previously created linval object
#'
#' @param x is this is the name of the previously created linval object.
#' @param geom geom to use. Guess automatically based on whether the predictor
#'   is on the y-axis (the default), or elsewhere.
#' @param hierarchy override the ordering of variables from the model.
#'   Combined with the original formula using \code{\link{update.formula}}
#'   so you use can \code{.} as a place holder.
#' @export
#' @examples
#' d_quantiles <- linval(price ~ carat + color + clarity, data = diamonds,
#'   reduction = "qgrid", bins = 4)
#'
#' autoplot(d_quantiles)
#'
#' # Use the second argument to rearrange the order of variables
#' autoplot(d_quantiles, . ~ color + clarity + carat)
#' autoplot(d_quantiles, color ~ clarity + price + .)
autoplot.linval <- function(x, hierarchy = . ~ ., geom = NULL) {
  formula <- update(x$formula, hierarchy)

  y <- all.vars(formula[[2]])
  xs <- all.vars(formula[[3]])

  if (length(xs) == 1) {
    # Use first x variable for x-axis
    aes <- aes_string(x = xs[[1]], y = y)
    facet <- facet_null()
  } else {
    # Second for grouping
    aes <- aes_string(x = xs[[1]], y = y, group = xs[[2]], colour = xs[[2]])

    # Remaining variables used for facetting
    if (length(xs) == 2) {
      facet <- facet_null()
    } else if (length(xs == 3)) {
      facet <- facet_wrap(xs[[3]])
    } else {
      left <- xs[-(1:2)]
      pieces <- split(left, seq_along(left) < length(left) / 2)

      formula <- paste0(
        paste0(pieces[[1]], collapse = " + "),
        " ~ ",
        paste0(pieces[[2]], collapse = " + ")
      )
      facet <- facet_grid(formula)
    }
  }

  # Figure out geom
  if (is.null(geom)) {
    x_num <- is.numeric(x$grid[[xs[1]]])
    y_num <- is.numeric(x$grid[[y]])

    # Predictor is on y-axis
    if (identical(y, x$y)) {
      if (x_num) {
        geom <- "line"
      } else {
        geom <- "point"
      }
    } else {
      geom <- "tile"
      aes$fill <- aes$colour
    }
  }

  ggplot(x$grid, aes) +
    layer(geom = geom) +
    facet
}
