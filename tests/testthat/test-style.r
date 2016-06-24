if (requireNamespace("lintr", quietly = TRUE)) {
  context("Code style")
  test_that("lint free", {
    lintr::expect_lint_free()
  })
}
