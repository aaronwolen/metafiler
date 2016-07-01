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
