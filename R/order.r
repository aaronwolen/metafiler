#' Reorder samples and features
#'
#' Reorder samples or features of an \code{ExpressionSet} object based on the
#' rank of their aggregated values. The manner in which each sample or feature's
#' values are aggregated is determined by \code{fun.aggregate}, which defaults
#' to \code{mean}.
#'
#' @param data \code{ExpressionSet} object.
#' @param fun.aggregate function that takes a vector of numbers and returns a
#'   single number (defaults to \code{mean}).
#' @param decreasing logical. Should the sort be increasing or decreasing?
#' @name reorder_dimensions
#' @return An \code{ExpressionSet} object reordered on one dimension.
#' @examples
#' reorder_samples(profiles)
#' reorder_features(profiles)
NULL

#' @export
#' @rdname reorder_dimensions
reorder_samples <-
  function(data, fun.aggregate = NULL, decreasing = TRUE) {
  UseMethod("reorder_samples")
}

#' @export
#' @rdname reorder_dimensions
reorder_samples.ExpressionSet <-
  function(data, fun.aggregate = NULL, decreasing = TRUE) {

  ordered.labels <- .reorder_dimension(
    Biobase::exprs(data),
    dimension = 1,
    fun.aggregate = fun.aggregate,
    decreasing = decreasing
  )

  data[ordered.labels, ]
}

#' @export
#' @rdname reorder_dimensions
reorder_features <-
  function(data, fun.aggregate = NULL, decreasing = TRUE) {
  UseMethod("reorder_features")
}

#' @export
#' @rdname reorder_dimensions
reorder_features.ExpressionSet <-
  function(data, fun.aggregate = NULL, decreasing = TRUE) {

  ordered.labels <- .reorder_dimension(
    Biobase::exprs(data),
    dimension = 2,
    fun.aggregate = fun.aggregate,
    decreasing = decreasing
  )

  data[, ordered.labels]
}


# reorder one dimension of a matrix based on rank of the scalar values returned
# by fun.aggregate and return the newly ordered labels
.reorder_dimension <-
  function(x, dimension, fun.aggregate = NULL, decreasing) {

  if (is.null(fun.aggregate)) {
    aggregates <- switch(as.character(dimension),
                        "1" = rowMeans(x),
                        "2" = colMeans(x))
  } else {
    aggregates <- apply(x, MARGIN = dimension, FUN = fun.aggregate)
  }

  names(sort(aggregates, decreasing = decreasing))
}
