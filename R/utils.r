# verify variable are present
check_vars <- function(x, variables) {
  present <- x %in% variables
  if (all(present)) return(invisible(x))
  stop(
    "The following variables were not found:\n",
    paste0("- ", x[!present], "\n"),
    call. = FALSE
  )
}

# consolidate features not among top.n into an "other" group
top_n_features <- function(data, n, label = "Other") {
  top.n <- min(n, nrow(data))

  fnames <- Biobase::featureNames(data)
  fnames <- replace(fnames, !fnames %in% fnames[seq_len(top.n)], label)

  values <- data.frame(Biobase::exprs(data), check.names = FALSE)
  pieces <- split(values, fnames, drop = FALSE)
  whole <- do.call("rbind", lapply(pieces, colSums))[unique(fnames), ]
  Biobase::exprs(data) <- as.matrix(whole)

  # replace metadata for consolidated features with new entry for Other
  data[unique(fnames), ]
}


# Assign a color to each unique value of x based on the supplied palette fxn
# replace is named vector
# returns a vector of colors named for values of x
map_colors <- function(x, palette, replace = NULL) {
  if (!is.function(palette))
    stop("palette must be a function that returns a vector of n colors.",
         call. = FALSE)
  x <- unique(x)
  n <- length(x)
  pal <- palette(n)

  # respect named colors
  if (!is.null(names(pal))) {
    named <- names(pal) != ""
    names(pal)[!named] <- setdiff(x, names(pal))
    out <- pal[x]
  } else {
    out <- stats::setNames(palette(n), nm = x)
  }

  replace <- replace[intersect(names(out), names(replace))]
  out[names(replace)] <- replace
  out
}
