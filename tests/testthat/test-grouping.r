context("Grouping")

out <- add_max_feature(eset, reorder.samples = FALSE)

test_that("respects reorder.samples argument", {
  expect_equal(Biobase::sampleNames(out), Biobase::sampleNames(eset))
})

test_that("group levels are based on feature with max value", {
  expect_is(out$.group, "factor")
  expect_equal(as.character(out$.group), c("f2", "f1", "f1"))
})

test_that("samples are ordered by group prevalence", {
  out <- add_max_feature(eset, reorder.samples = TRUE)
  expect_is(out$.group, "factor")
  expect_equal(Biobase::sampleNames(out), c("s2", "s3", "s1"))
})
