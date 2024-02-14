context("Profile dotplot")

eset2 <- eset[, 1:2]

test_that("assigns a unique color to each feature", {
  p <- profile_dotplot(eset2, color = "feature")
  pdata <- ggplot2::ggplot_build(p)$data[[1]]
  expect_equivalent(length(unique(pdata$colour)), nrow(eset2))
})

test_that("assigns a unique color to each sample", {
  p <- profile_dotplot(eset2, color = "sample")
  pdata <- ggplot2::ggplot_build(p)$data[[1]]
  expect_equivalent(length(unique(pdata$colour)), ncol(eset2))
})

test_that("assigns a unique color to levels of fData variable", {
  Biobase::fData(eset2)$x <- letters[1:3]
  p <- profile_dotplot(eset2, color = "x")
  pdata <- ggplot2::ggplot_build(p)$data[[1]]
  expect_equivalent(length(unique(pdata$colour)), nrow(eset2))
})

test_that("assigns a unique color to levels of pData variable", {
  Biobase::pData(eset2)$y <- letters[1:2]
  p <- profile_dotplot(eset2, color = "y")
  pdata <- ggplot2::ggplot_build(p)$data[[1]]
  expect_equivalent(length(unique(pdata$colour)), ncol(eset2))
})

test_that("respects top.n argument", {
  p <- profile_dotplot(eset, top.n = 1)
  pdata <- ggplot2::ggplot_build(p)$data[[1]]
  expect_equal(max(pdata$y), 2)

  ylabs <- ggplot2::ggplot_build(p)$layout$panel_ranges[[1]]$y.labels
  expect_equal(ylabs, c("f1", "Other"))
})
