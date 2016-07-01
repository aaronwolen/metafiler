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
#'
#' @examples
#' profiles <- add_max_feature(profiles, group = "enterotype")
#' profiles$enterotype
#'
#' @export

add_max_feature <-
  function(data, group = ".group", reorder.samples = TRUE) {
  UseMethod("add_max_feature")
}

#' @export
add_max_feature.ExpressionSet <-
  function(data, group = ".group", reorder.samples = TRUE) {

  values <- Biobase::exprs(data)

  # sample group assignment
  max.i  <- apply(values, MARGIN = 2, which.max)
  sample.group <- Biobase::featureNames(data)[max.i]

  # order by prevalence
  group.n <- sort(table(sample.group), decreasing = TRUE)
  sample.group <- factor(sample.group, levels = names(group.n))

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
