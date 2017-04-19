#' Stacked barplot of sample feature profiles
#'
#' The \code{top.n} argument consolidates all features not among the
#' \code{top.n} into a single feature that is named "Other". The order of
#' features within the \code{ExpressionSet} object is used to determine whether
#' features are among the \code{top.n}. The \code{reorder_features()} function
#' can be used to change this order with respect to the rank of their aggregated
#' values. By default all features are visualized. However, for cases where
#' there are many features with relatively values, utilizing the \code{top.n}
#' arugment can speed up plot rendering without sacrificing visual information.
#'
#' @inheritParams add_max_feature
#' @inheritParams ggplot2::geom_bar
#' @param palette a function that generates \code{n} colors.
#' @param legend include a legend?
#' @param top.n Number of features to visualize and color as unique entities.
#' @param order.features Arrange features in ascending order within each sample.
#' @param other.color Color applied to features not among the \code{top.n}.
#'
#' @examples
#' profile_barplot(profiles)
#'
#' @export
#' @importFrom ggplot2 ggplot aes_string geom_col
#' @importFrom Biobase pData "pData<-" fData "fData<-" featureNames
#'   "featureNames<-" sampleNames "sampleNames<-" exprs "exprs<-"
#' @export pData "pData<-" fData "fData<-" featureNames "featureNames<-"
#' @export sampleNames "sampleNames<-" exprs "exprs<-"
#'

profile_barplot <-
  function(data,
           palette,
           legend = FALSE,
           top.n = NULL,
           order.features = FALSE,
           other.color = "grey50",
           width = 0.9) {
  UseMethod("profile_barplot")
}

#' @export
profile_barplot.ExpressionSet <-
  function(data,
           palette,
           legend = FALSE,
           top.n = NULL,
           order.features = FALSE,
           other.color = "grey50",
           width = 0.9) {

  # check whether the sum of every sample's feature values is 1 (or 100)
  values <- Biobase::exprs(data)

  if ( all(colSums(values) == 1) | all(colSums(values) == 100) ) {
    feature.position <- ggplot2::position_stack(reverse = TRUE)
  } else {
    feature.position <- ggplot2::position_fill(reverse = TRUE)
    message(
      "Standardizing values  because 1 or more samples did not total to 1"
    )
  }

  if (missing(palette)) palette <- brewer_pal_plus(palette = "Set1")
  if (!is.null(top.n)) data <- top_n_features(data, top.n)

  colors <- map_colors(featureNames(data), palette, c(Other = other.color))
  plot.data <- to_dataframe(data)

  if (order.features) {
    plot.data <- plyr::ddply(
        .data = plot.data,
        .variables = "sample",
        .fun = plyr::mutate,
        group = plyr::desc(rank(value, ties.method = "first"))
      )
  } else {
    plot.data$group <- as.numeric(plot.data$sample)
  }

  ggplot(plot.data) +
    aes_string("sample", "value", fill = "feature", group = "group") +
    ggplot2::geom_col(
      position = feature.position,
      width = width,
      show.legend = legend
    ) +
    ggplot2::scale_fill_manual(values = colors, breaks = names(colors)) +
    ggplot2::scale_x_discrete(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    theme_metafiler()
}
