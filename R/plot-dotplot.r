#' Dotplot of sample feature profiles
#'
#' Dot size is determined by mapping point area to feature values. Dots can be
#' color coded by sample (\code{color = "sample"}), by feature (\code{color =
#' "feature"}) or any associated metadatda variable.
#'
#' @inheritParams add_max_feature
#' @inheritParams profile_barplot
#' @param color color code dots by sample, feature or any of the associated metadata variables.
#' @param max.size Size of largest dots.
#' @param title.area Title of point area legend.
#' @param title.color Title of point color legend.
#'
#' @examples
#' profile_dotplot(profiles)
#' @export
#' @importFrom ggplot2 geom_point scale_size_area scale_color_manual

profile_dotplot <-
  function(data,
           color = NULL,
           legend = TRUE,
           top.n = NULL,
           other.color = "grey50",
           max.size = 6,
           title.area,
           title.color) {
  UseMethod("profile_dotplot")
}

#' @export
profile_dotplot.ExpressionSet <-
  function(data,
           color = NULL,
           legend = TRUE,
           top.n = NULL,
           other.color = "grey50",
           max.size = 6,
           title.area,
           title.color) {

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

  plot.data <- to_dataframe(data)

  if (!is.null(color)) {
    vars <- setdiff(colnames(plot.data), "value")
    check_vars(color, vars)

    color.levels <- levels(factor(plot.data[[color]]))
    colors <- color_brewer_plus(palette = "Set1")[seq_along(color.levels)]
    colors <- stats::setNames(colors, color.levels)
    if (!is.null(top.n)) colors["Other"] <- other.color
    point.aes <- aes_string(size = "value", color = color)
  } else {
    point.aes <- aes_string(size = "value")
  }

  if (missing(title.area)) title.area <- "value"

  p <- ggplot(plot.data) +
    aes_string("sample", "feature") +
    geom_point(
      point.aes,
      show.legend = legend
    ) +
    ggplot2::scale_size_area(title.area, max_size = max.size) +
    theme_metafiler()

  if (!is.null(color)) {
    check_vars(color, colnames(plot.data))
    if (missing(title.color)) title.color <- color
    p <- p +
      ggplot2::scale_color_manual(title.color,
                                  values = colors,
                                  breaks = names(colors))
  }
  p
}
