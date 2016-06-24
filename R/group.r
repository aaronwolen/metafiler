#' Create a new group variable based on the highest feature
#'
#' Each \code{sample} is assigned to a new grouping variable (default:
#' \code{.group}) named for the \code{feature} with the maximum \code{value}.
#' Furthermore, the \code{sample} column is returned as a factor ordered by the
#' prevalence of its assigned group and rank of the assigned \code{feature}'s
#' \code{value} among other samples in that group.
#'
#' @param data \code{data.frame} or \code{tbl_df} containing relevant data
#' @param sample sample identifier column
#' @param feature feature identifier column
#' @param value column that stores values
#' @param group name of new grouping variable
#'
#' @examples
#' assign_max_group(profiles, 'sample', 'taxa', 'abundance')
#'
#' @export
#' @importFrom lazyeval interp
#' @importFrom magrittr '%>%'
#' @importFrom dplyr summarise_ mutate_ select_ arrange_ rename_ group_by_ filter_

assign_max_group <- function(data, sample, feature, value, group = '.group') {


  # return group row index corresponding to the highest value
  filter_max <- interp(~dplyr::row_number(dplyr::desc(v)) == 1, v = as.name(value))

  # sample group assignment
  groups <- data %>%
    group_by_(.dots = sample) %>%
    filter_(filter_max) %>%
    rename_(.dots = stats::setNames(list(feature), group)) %>%
    dplyr::ungroup() %>%
    order_by_prevalence(variable = group)

  data <- groups %>%
    select_(.dots = list(sample, group)) %>%
    dplyr::inner_join(data, by = sample)

  # order samples by group and value within group
  feature_groups <- interp(~ as.character(f) == as.character(g),
                           f = as.name(feature),
                           g = as.name(group))
  samples <- data %>%
    filter_(.dots = feature_groups) %>%
    order_by_rank(sample, value, by = group)

  data[[sample]] <- factor(data[[sample]], levels(samples[[sample]]))
  data
}




