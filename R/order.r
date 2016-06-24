#' Convert variables to ordered factors
#'
#' Convert \code{variable} to a factor ordered by the rank of \code{value}. If
#' \code{variable} does not uniquely identify a value, an aggregating function,
#' \code{fun.aggregate}, is applied.
#'
#' A grouping variable passed to \code{by} will cause rankings to be assigned
#' within each level of the grouping variable.
#'
#' @inheritParams assign_max_group
#' @param variable with \code{data} to be ordered
#' @param direction determines ordering direction. If 1, the default, the
#'   variable is ordered by descending prevalence.  If -1, the order is reversed.
#' @param by a column name in \code{data}. If supplied, \code{value} rankings
#'   are assigned within each level of \code{by}.
#' @param fun.aggregate function that takes a
#' vector of numbers and returns a single number; the default is \code{mean()}.
#' @export
#'
#' @examples
#' order_by_rank(profiles, 'taxa', 'abundance')

order_by_rank <- function(data, variable, value, by = NULL, fun.aggregate = NULL, direction = 1) {
  if (!direction %in% c(1, -1)) stop('direction must be 1 or -1', call. = FALSE)

  data[[variable]] <- as.character(data[[variable]])
  if (!is.null(by)) data <- dplyr::group_by_(data, by)

  # aggregate duplicates
  if (is_unique(data, variable)) {
    ranks <- data
  } else {
    if (is.null(fun.aggregate)) {
      message("Aggregation function missing: defaulting to mean")
      fun.aggregate <- mean
    }
    aggregate_dupes <- interp(~ fun.aggregate(v), v = as.name(value))
    ranks <- data %>%
      dplyr::group_by_(variable, add = TRUE) %>%
      dplyr::summarise_(.dots = stats::setNames(list(aggregate_dupes), value)) %>%
      dplyr::ungroup()
  }

  # return variable rank in descending order
  value_rank <- switch(as.character(direction),
     '1' = interp(~ dplyr::row_number(dplyr::desc(v)), v = as.name(value)),
    '-1' = interp(~ dplyr::row_number(v),              v = as.name(value))
  )

  ranks <- mutate_(ranks, .dots = list(rank = value_rank))

  if (is.null(by)) {
    ranks <- dplyr::arrange_(ranks, .dots = 'rank')
  } else {
    ranks <- dplyr::arrange_(ranks, .dots = c(by, 'rank'))
  }

  data[[variable]] <- factor(data[[variable]], levels = unique(ranks[[variable]]))
  data
}



#' Order a variable by its prevalence in the data
#'
#' The order of the data is not actually changed. Rather, the specified variable
#' is converted to a factor ordered by the number of times it is observed in the
#' data.
#'
#' @inheritParams assign_max_group
#' @inheritParams order_by_rank
#' @export
order_by_prevalence <- function(data, variable, direction = 1) {
  if (!direction %in% c(1, -1)) stop('direction must be 1 or -1', call. = FALSE)

  var_order <- switch(as.character(direction),
                      '1'  = interp( ~ dplyr::desc(n)),
                      '-1' = 'n')

  counts <- dplyr::count_(data, variable)
  ranks  <- dplyr::arrange_(counts, .dots = var_order)
  data[[variable]] <- factor(data[[variable]], ranks[[variable]])
  data
}

