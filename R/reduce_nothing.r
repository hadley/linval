#' Leave the data as is.
#'
#' @param data input data
#' @export
#' @examples
#' reduce_nothing(diamonds[1:3])
reduce_nothing <- function(data, ...) {
  data
}
