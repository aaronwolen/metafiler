context("Profile plot")

test_that("throws message about non-standardized values", {
  expect_message(profile_barplot(eset), "Standardizing values")

  eset <- Biobase::ExpressionSet(apply(mat, 2, function(x) x / sum(x)))
  expect_silent(profile_barplot(eset))
})
