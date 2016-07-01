context("ExpressionSet to data.frame coercion")

Biobase::pData(eset)$pcol <- letters[1:3]
Biobase::fData(eset)$fcol <- letters[4:6]

test_that("maintains sample/feature order", {
  out <- to_dataframe(eset)

  expect_is(out$sample, "factor")
  expect_equal(levels(out$sample), Biobase::sampleNames(eset))

  expect_is(out$feature, "factor")
  expect_equal(levels(out$feature), Biobase::featureNames(eset))
})

test_that("includes appropriate metadata", {
  out <- to_dataframe(eset)
  expect_equal(colnames(out), c("feature", "sample", "value", "pcol", "fcol"))

  out <- to_dataframe(eset, add.pvars = FALSE)
  expect_equal(colnames(out), c("feature", "sample", "value", "fcol"))

  out <- to_dataframe(eset, add.fvars = FALSE)
  expect_equal(colnames(out), c("feature", "sample", "value", "pcol"))

  out <- to_dataframe(eset, add.pvars = FALSE, add.fvars = FALSE)
  expect_equal(colnames(out), c("feature", "sample", "value"))
})

test_that("maintains metadata integrity", {
  out <- to_dataframe(eset)
  expect_equal(
    unlist(split(out$pcol, out$sample), use.names = FALSE),
    rep(letters[1:3], each = 3)
  )
  expect_equal(
    unlist(split(out$fcol, out$feature), use.names = FALSE),
    rep(letters[4:6], each = 3)
  )
})


context("ExpressionSet from data.frame coercion")

Biobase::fData(eset)$fcol2 <- letters[7:9]
df <- to_dataframe(eset)

test_that("maintains sample/feature order", {
  out <- from_dataframe(df, "sample", "feature", "value")
  expect_s4_class(out, "ExpressionSet")
  expect_equal(Biobase::sampleNames(out),  levels(df$sample))
  expect_equal(Biobase::featureNames(out), levels(df$feature))
})

test_that("includes appropriate metadata", {
  out <- from_dataframe(df, "sample", "feature", "value")
  expect_equal(colnames(Biobase::pData(out)), character())
  expect_equal(colnames(Biobase::fData(out)), character())

  out <- from_dataframe(df, "sample", "feature", "value", pvars = "pcol")
  expect_equal(colnames(Biobase::pData(out)), "pcol")

  out <- from_dataframe(df, "sample", "feature", "value",
                        fvars = c("fcol", "fcol2"))
  expect_equal(colnames(Biobase::fData(out)), c("fcol", "fcol2"))
})
