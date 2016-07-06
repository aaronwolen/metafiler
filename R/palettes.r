
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
