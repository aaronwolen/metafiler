
# vector of available colors without similar variants and near-white colors
.colors <- setdiff(
  x = unique(
    sub(
      pattern = "\\d{1,2}$",
      replacement = "",
      x = grDevices::colors(distinct = TRUE)
    )
  ),
  y = c(
    "white",
    "ivory",
    "aliceblue",
    "antiquewhite",
    "azure",
    "beige",
    "bisque",
    "blanchedalmond",
    "cornsilk"
  )
)

#' Augmented color palette functions
#'
#' Palette functions return a function that generates colors. These
#' \code{_plus()} functions are designed to work like their counterparts in the
#' \code{scales} package, with the exception that additional standard colors are
#' appended to the original palettes. The additional colors are not pretty but
#' some effort has been made to avoid overly similar colors.
#' @inheritParams scales::brewer_pal
#' @inheritParams scales::manual_pal
#' @name palettes
NULL

#' @export
#' @rdname palettes
brewer_pal_plus <- function(palette = 1, direction = 1) {
  palettes <- RColorBrewer::brewer.pal.info

  if (is.numeric(palette)) {
    if (palette > nrow(palettes))
      stop("There are only ", nrow(palettes), " ColorBrewer palettes",
           call. = FALSE)
    pal <- rownames(palettes)[palette]
  } else {
    pal <- match.arg(palette, rownames(palettes))
  }

  function(n) {
    brew.n <- palettes[pal, "maxcolors"]
    colors <- RColorBrewer::brewer.pal(brew.n, pal)
    if (direction == -1) colors <- rev(colors)
    c(colors, .colors)[seq_len(n)]
  }
}

#' @export
#' @rdname palettes
manual_pal_plus <- function(values) {
  function(n) {
    colors <- values
    c(colors, .colors)[seq_len(n)]
  }
}
