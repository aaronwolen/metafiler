#' Assign samples to a new group variable based on the highest feature
#'
# ' Each sample is assigned to a new grouping variable (default: '
# \code{.group}) named for the feature with the maximum value.
#'
#' @param data \code{ExpressionSet} object.
#' @param group name of new grouping variable.
#' @param reorder.samples If \code{TRUE}, samples are reordered by the
#'   prevalance of their assigned group and rank of the assigned feature's value
#'   within the group.
#' @param top.n Limit the number of levels in the new grouping variable to the
#'   \code{top.n} most prevalent. Samples not among the most prevalent groups
#'   are assigned to \code{"Othere"}.
#'
#' @examples
#' profiles <- add_max_feature(profiles, group = "enterotype")
#' profiles$enterotype
#'
#' @export

add_max_feature <-
  function(data, group = ".group", reorder.samples = TRUE, top.n = NULL) {
  UseMethod("add_max_feature")
}

#' @export
add_max_feature.ExpressionSet <-
  function(data, group = ".group", reorder.samples = TRUE, top.n = NULL) {

  values <- Biobase::exprs(data)

  # sample group assignment
  max.i  <- apply(values, MARGIN = 2, which.max)
  sample.group <- Biobase::featureNames(data)[max.i]

  # order by prevalence
  groups <- most_frequent(sample.group)

  if (!is.null(top.n)) {
    top.n <- min(top.n, length(groups))
    groups <- c(groups[seq_len(top.n)], "Other")
    sample.group <- replace(sample.group, !sample.group %in% groups, "Other")
  }

  sample.group <- factor(sample.group, levels = groups)

  Biobase::phenoData(data)[[group]] <- sample.group
  if (!reorder.samples) return(data)

  # order samples by group and value within group
  value.index <- cbind(
    row = match(sample.group, Biobase::featureNames(data)),
    col = seq_len(ncol(data))
  )

  sample.value <- values[value.index]
  group.order <- order(sample.group, -xtfrm(sample.value))

  data[, group.order]
}
