context("Utilities")

test_that("check_vars identifies missing variables", {
  x <- letters[1:5]
  expect_silent(check_vars(c("a", "b"), x))
  expect_error(check_vars(c("1", "a", "b"), x), regexp = "\n- 1")
})


test_that("top_n_features consolidated bottom features", {
  top.n <- 1
  out <- top_n_features(eset, top.n)
  expect_equivalent(nrow(out), top.n + 1)
  expect_equal(Biobase::featureNames(out), c("f1", "Other"))
  expect_equivalent(Biobase::exprs(out)["Other", ], colSums(mat[2:3, ]))
})
