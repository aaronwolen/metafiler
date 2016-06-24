# Check if variable is a unique identifier within a data.frame or within the
# group(s) of a grouped data.frame
is_unique <- function(data, variable) {
  ids <- list(data[[variable]], dplyr::group_indices(data))
  dplyr::n_distinct(dplyr::id(ids)) == nrow(data)
}
