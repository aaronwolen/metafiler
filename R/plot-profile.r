#' Stacked bar plot of profile samples by features
#' @inheritParams assign_max_group
#' @param legend include a legend?
#'
#' @examples
#' plot_profile(profiles, 'sample', 'taxa', 'abundance')
#'
#' @export
#' @importFrom ggplot2 ggplot aes_string geom_bar

plot_profile <- function(data, sample, feature, value, legend = FALSE) {

  # check whether the sum of every sample's feature values is 1
  if (all(tapply(data[[value]], data[[sample]], sum) == 1)) {
    feature.position <- "stack"
  } else {
    feature.position <- "fill"
    message(
      "Standardizing values (", value, ") because 1 or more ",
      sample, "s did not total to 1"
    )
  }

  if (is.character(data[[feature]])) data[[feature]] <- factor(data[[feature]])

  colors <- color_brewer_plus(palette = "Set1")
  colors <- stats::setNames(colors[1:nlevels(data[[feature]])],
                            levels(data[[feature]]))

  ggplot(data) +
    aes_string(sample, value, fill = feature) +
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
