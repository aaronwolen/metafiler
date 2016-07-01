context("Reordering")

test_that("rows is correct", {
  out <- .reorder_dimension(mat, 1, decreasing = TRUE)
  expect_equal(out,  c("f1", "f2"))
  out <- .reorder_dimension(mat, 1, decreasing = FALSE)
  expect_equal(out, c("f2", "f1"))
})

test_that("columns is correct", {
  out <- .reorder_dimension(mat, 2, decreasing = TRUE)
  expect_equal(out,  c("s2", "s1", "s3"))
  out <- .reorder_dimension(mat, 2, decreasing = FALSE)
  expect_equal(out, c("s1", "s3", "s2"))
})
