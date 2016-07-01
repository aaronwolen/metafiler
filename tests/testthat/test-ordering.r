context("Reordering")

test_that("matrix rows is correct", {
  out <- .reorder_dimension(mat, 1, decreasing = TRUE)
  expect_equal(out,  c("f3", "f1", "f2"))
  out <- .reorder_dimension(mat, 1, decreasing = FALSE)
  expect_equal(out, c("f2", "f1", "f3"))
})

test_that("matrix columns is correct", {
  out <- .reorder_dimension(mat, 2, decreasing = TRUE)
  expect_equal(out, c("s3", "s2", "s1"))
  out <- .reorder_dimension(mat, 2, decreasing = FALSE)
  expect_equal(out, c("s1", "s2", "s3"))
})


test_that("ExpressionSet features", {
  out <- reorder_features(eset, decreasing = TRUE)
  expect_equal(Biobase::featureNames(out), c("f3", "f1", "f2"))
  out <- reorder_features(eset, decreasing = FALSE)
  expect_equal(Biobase::featureNames(out), c("f2", "f1", "f3"))
})

test_that("ExpressionSet samples", {
  out <- reorder_samples(eset, decreasing = TRUE)
  expect_equal(Biobase::sampleNames(out), c("s3", "s2", "s1"))
  out <- reorder_samples(eset, decreasing = FALSE)
  expect_equal(Biobase::sampleNames(out), c("s1", "s2", "s3"))
})

test_that("uses fun.aggregate", {
  out <- reorder_features(eset, fun.aggregate = mean)
  expect_equal(Biobase::featureNames(out), c("f3", "f1", "f2"))
  out <- reorder_features(eset, fun.aggregate = median)
  expect_equal(Biobase::featureNames(out), c("f1", "f3", "f2"))
})
