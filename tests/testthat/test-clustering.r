context("Clustering")

test_that("matrix rows is correct", {
  out <- .cluster_dimension(mat, 1, "euclidean", "average")
  clust <- stats::hclust(stats::dist(mat, "euclidean"), "average")
  expect_equal(out, clust$labels[clust$order])
})

test_that("matrix columns is correct", {
  out <- .cluster_dimension(mat, 2, "euclidean", "average")
  clust <- stats::hclust(stats::dist(t(mat), "euclidean"), "average")
  expect_equal(out, clust$labels[clust$order])
})


test_that("ExpressionSet features", {
  out <- cluster_features(eset)
  expect_equal(Biobase::featureNames(out), c("f3", "f1", "f2"))
  expect_equal(Biobase::sampleNames(out), Biobase::sampleNames(eset))
})

test_that("ExpressionSet samples", {
  out <- cluster_samples(eset)
  expect_equal(Biobase::sampleNames(out), c("s3", "s1", "s2"))
  expect_equal(Biobase::featureNames(out), Biobase::featureNames(eset))
})
