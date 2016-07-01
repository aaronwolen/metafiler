context("Profile barplot")

test_that("throws message about non-standardized values", {
  expect_message(profile_barplot(eset), "Standardizing values")

  eset <- Biobase::ExpressionSet(apply(mat, 2, function(x) x / sum(x)))
  expect_silent(profile_barplot(eset))
})


test_that("assigns a unique color to each feature", {
  suppressMessages(p <- profile_barplot(eset))
  colors <- unique(ggplot2::ggplot_build(p)$data[[1]]$fill)
  colors <- setdiff(colors, "grey50")
  expect_equivalent(length(colors), nrow(eset))
})

test_that("respects top.n argument", {
  suppressMessages(p <- profile_barplot(eset, top.n = 2))
  colors <- unique(ggplot2::ggplot_build(p)$data[[1]]$fill)
  colors <- setdiff(colors, "grey50")
  expect_equivalent(length(colors), 2)
})

test_that("handles excessive top.n arguments", {
  suppressMessages(p <- profile_barplot(eset, top.n = nrow(eset) + 1))
  colors <- unique(ggplot2::ggplot_build(p)$data[[1]]$fill)
  colors <- setdiff(colors, "grey50")
  expect_equivalent(length(colors), nrow(eset))
})
