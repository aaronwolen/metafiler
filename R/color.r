
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


color_brewer_plus <- function(palette = 1, direction = 1) {

  palettes <- RColorBrewer::brewer.pal.info

  if (is.numeric(palette)) {
    if (palette > nrow(palettes))
      stop("There are only ", nrow(palettes), " ColorBrewer palettes",
           call. = FALSE)
    palette <- rownames(palettes)[palette]
  } else {
    palette <- match.arg(palette, rownames(palettes))
  }

  n <- palettes[palette, "maxcolors"]
  colors <- scales::brewer_pal(palette = palette, direction = direction)(n)
  c(colors, .colors)
}
