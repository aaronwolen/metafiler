context("Grouping")

out <- add_max_feature(eset, reorder.samples = FALSE)

test_that("respects reorder.samples argument", {
  expect_equal(Biobase::sampleNames(out), Biobase::sampleNames(eset))
})

test_that("group levels are based on feature with max value", {
  expect_is(out$.group, "factor")
  expect_equal(as.character(out$.group), c("f3", "f1", "f3"))
})

test_that("samples are ordered by group prevalence", {
  out <- add_max_feature(eset, reorder.samples = TRUE)
  expect_is(out$.group, "factor")
  expect_equal(Biobase::sampleNames(out), c("s3", "s1", "s2"))
})

test_that("sample not in top.n groups are assigned to Other", {
  out <- add_max_feature(eset, reorder.samples = TRUE, top.n = 1)
  expect_equal(c("f3", "Other"), levels(out$.group))
})
