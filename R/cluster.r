#' Reorder samples and features by hierarchical clustering
#'
#' Reorder samples or features of an \code{ExpressionSet} object based on
#' agglomerative hierarchical clustering of their profiles.
#'
#' Distance among samples or features is first calculated using the method
#' specified by \code{dist.method}, which can be any of the measures accepted by
#' \code{\link[stats]{dist}}. Similarly, \code{hclust.method} can be any of the
#' agglomeration methods accepted by \code{\link[stats]{hclust}}.
#'
#' @param data \code{ExpressionSet} object.
#' @param dist.method distance metric to be calculated prior to clustering.
#' @param hclust.method the agglomeration method to be used.
#' @name cluster_dimensions
#' @return An \code{ExpressionSet} object reordered on one dimension.
#' @examples
#' cluster_samples(profiles)
#' cluster_features(profiles)
#'
#' @importFrom stats dist hclust
#' @seealso \code{\link[stats]{dist}}, \code{\link[stats]{hclust}}
NULL

#' @export
#' @rdname cluster_dimensions
cluster_samples <-
  function(data, dist.method = "euclidean", hclust.method = "average") {
  UseMethod("cluster_samples")
}

#' @export
#' @rdname cluster_dimensions
cluster_samples.ExpressionSet <-
  function(data, dist.method = "euclidean", hclust.method = "average") {

  ordered.labels <- .cluster_dimension(
    Biobase::exprs(data),
    dimension = 2,
    dist.method = dist.method,
    hclust.method = hclust.method
  )

  data[, ordered.labels]
}

#' @export
#' @rdname cluster_dimensions
cluster_features <-
  function(data, dist.method = "euclidean", hclust.method = "average") {
  UseMethod("cluster_features")
}

#' @export
#' @rdname cluster_dimensions
cluster_features.ExpressionSet <-
  function(data, dist.method = "euclidean", hclust.method = "average") {

  ordered.labels <- .cluster_dimension(
    Biobase::exprs(data),
    dimension = 1,
    dist.method = dist.method,
    hclust.method = hclust.method
  )

  data[ordered.labels, ]
}


# reorder one dimension of a matrix based on the order returned by hclust and
# returns labels of reordered dimension
.cluster_dimension <-
  function(data, dimension, dist.method, hclust.method) {
    distances <- switch(as.character(dimension),
                        "1" = stats::dist(data, method = dist.method),
                        "2" = stats::dist(t(data), method = dist.method))
    clustered <- stats::hclust(distances, method = hclust.method)
    clustered$labels[clustered$order]
}
