context("Grouping")

df <- dplyr::data_frame(
  s = rep(letters[1:3], each = 2),
  f = rep(c("f1", "f2"), 3),
  v = c(1, 2, 2, 1, 4, 3)
)

out <- assign_max_group(df, "s", "f", "v")

test_that("group levels are based on feature with max value", {
  expect_is(out$.group, "factor")
  expect_equal(as.character(out$.group), c("f2", "f2", "f1", "f1", "f1", "f1"))
})

test_that("sample levels are ordered by group prevalence", {
  expect_is(out$s, "factor")
  expect_equal(levels(out$s), c("c", "b", "a"))
})
