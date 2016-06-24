#' Default profile barplot theme
#'
#' @inheritParams ggplot2::theme_grey
#' @importFrom ggplot2 "%+replace%" element_text element_line element_rect element_blank
#' @export
theme_metafiler <- function(base_size = 12, base_family = "") {
  ggplot2::theme_bw(base_size = base_size, base_family = base_family) %+replace%
    ggplot2::theme(
      panel.grid = element_blank(),
      axis.title  = element_text(face = "bold"),
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
    )
}
