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
#' @param legend include a legend?
#' @param top.n Number of features to visualize and color as unique entities.
#' @param other.color Color applied to features not among the \code{top.n}.
#' @param @inheritParams ggplot2::geom_bar
#'
#' @examples
#' profile_barplot(profiles)
#'
#' @export
#' @importFrom ggplot2 ggplot aes_string geom_bar
#' @importFrom Biobase pData "pData<-" fData "fData<-" featureNames
#'   "featureNames<-" sampleNames "sampleNames<-" exprs "exprs<-"
#' @export pData "pData<-" fData "fData<-" featureNames "featureNames<-"
#' @export sampleNames "sampleNames<-" exprs "exprs<-"
#'

profile_barplot <-
  function(data,
           legend = FALSE,
           top.n = NULL,
           other.color = "grey50",
           width = 0.9) {
  UseMethod("profile_barplot")
}

#' @export
profile_barplot.ExpressionSet <-
  function(data,
           legend = FALSE,
           top.n = NULL,
           other.color = "grey50",
           width = 0.9) {

  # check whether the sum of every sample's feature values is 1 (or 100)
  values <- Biobase::exprs(data)

  if ( all(colSums(values) == 1) | all(colSums(values) == 100) ) {
    feature.position <- "stack"
  } else {
    feature.position <- "fill"
    message(
      "Standardizing values  because 1 or more samples did not total to 1"
    )
  }

  # consolidate features not among top.n into an "other" group
  if (!is.null(top.n)) {
    top.n <- min(top.n, nrow(data))
    fnames <- Biobase::featureNames(data)
    fnames <- replace(fnames, !fnames %in% fnames[seq_len(top.n)], "Other")

    pieces <- split(data.frame(Biobase::exprs(data)), fnames, drop = FALSE)
    whole <- do.call("rbind", lapply(pieces, colSums))[unique(fnames), ]
    Biobase::exprs(data) <- as.matrix(whole)

    # replace metadata for consolidated features with new entry for Other
    data <- data[unique(fnames), ]
  }

  colors <- color_brewer_plus(palette = "Set1")
  colors <- stats::setNames(colors[1:nrow(data)], Biobase::featureNames(data))
  if (!is.null(top.n)) colors["Other"] <- other.color

  plot.data <- to_dataframe(data)

  ggplot(plot.data) +
    aes_string("sample", "value", fill = "feature") +
    geom_bar(
      stat = "identity",
      position = feature.position,
      width = width,
      show.legend = legend
    ) +
    ggplot2::scale_fill_manual(values = colors, breaks = names(colors)) +
    ggplot2::scale_x_discrete(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    theme_metafiler()
}
