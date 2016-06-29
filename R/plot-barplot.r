#' Stacked bar plot of sample feature profiles
#' @inheritParams add_max_feature
#' @param legend include a legend?
#'
#' @examples
#' profile_barplot(profiles)
#'
#' @export
#' @importFrom ggplot2 ggplot aes_string geom_bar
#' @importFrom Biobase pData fData featureNames sampleNames
#'

profile_barplot <- function(data, legend = FALSE) {
  UseMethod("profile_barplot")
}

#' @export
profile_barplot.ExpressionSet <- function(data, legend = FALSE) {

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

  colors <- color_brewer_plus(palette = "Set1")
  colors <- stats::setNames(colors[1:nrow(data)], Biobase::featureNames(data))

  plot.data <- to_dataframe(data)

  ggplot(plot.data) +
    aes_string("sample", "value", fill = "feature") +
    geom_bar(
      stat = "identity",
      position = feature.position,
      show.legend = legend
    ) +
    ggplot2::scale_fill_manual(values = colors, breaks = names(colors)) +
    ggplot2::scale_x_discrete(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    theme_metafiler()
}
