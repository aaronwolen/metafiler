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

  pieces <- split(data.frame(Biobase::exprs(data)), fnames, drop = FALSE)
  whole <- do.call("rbind", lapply(pieces, colSums))[unique(fnames), ]
  Biobase::exprs(data) <- as.matrix(whole)

  # replace metadata for consolidated features with new entry for Other
  data[unique(fnames), ]
}
