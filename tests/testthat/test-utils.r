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

test_that("top_n_features does not munge feature/sample names", {
  eset2 <- eset
  Biobase::featureNames(eset2) <- paste0(Biobase::featureNames(eset2), "-a")
  Biobase::sampleNames(eset2)  <- paste0(Biobase::sampleNames(eset2),  "-b")
  out <- top_n_features(eset2, 2)
  out.mat <- Biobase::exprs(out)
  # features
  expect_equal(Biobase::featureNames(out), c("f1-a", "f2-a", "Other"))
  expect_equal(rownames(out.mat), Biobase::featureNames(out))
  # samples
  expect_equal(Biobase::sampleNames(out), Biobase::sampleNames(eset2))
  expect_equal(colnames(out.mat), Biobase::sampleNames(out))
})

test_that("expects a palette function", {
  expect_error(map_colors(letters[1:5], colors()[1:5]), "palette must")
  expect_silent(map_colors(letters[1:5], topo.colors))
})

test_that("map_colors assigns colors to unique values", {
  x <- letters[1:5]
  out <- map_colors(x, topo.colors)
  expect_equal(length(out), length(x))
  expect_equal(names(out), x)
})

test_that("map_colors respects replacement colors", {
  x <- letters[1:5]
  out <- map_colors(x, topo.colors, c(e = "red"))
  expect_equal(out["e"], c(e = "red"))
  out <- map_colors(x, topo.colors, c(f = "red"))
  expect_equal(out, map_colors(x, topo.colors))
})

test_that("map_colors ignores absent replacement colors", {
  x <- letters[1:5]
  out <- map_colors(x, topo.colors, c(f = "red"))
  expect_equal(out, map_colors(x, topo.colors))
})

test_that("map_colors respects named color vectors", {
  x <- letters[1:5]
  colors <- c(b = "orange", d = "yellow")
  f <- manual_pal_plus(colors)
  out <- map_colors(x, f)
  expect_equal(out[c("b", "d")], colors)
})
