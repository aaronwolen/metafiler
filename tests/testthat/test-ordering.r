context("Ordering by rank")

df <- dplyr::data_frame(
  s = paste0("s", 1:3),
  v = 1:3
)

test_that("returns a correctly ordered factor", {
  out <- order_by_rank(df, "s", "v")
  expect_is(out$s, "factor")
  expect_equal(levels(out$s), c("s3", "s2", "s1"))
  expect_equal(as.character(out$s), df$s)
})

test_that("respects direction", {
  out <- order_by_rank(df, "s", "v", direction = -1)
  expect_equal(levels(out$s), c("s1", "s2", "s3"))
})

test_that("aggregates values for duplicate variable levels", {
  df <- dplyr::bind_rows(dplyr::data_frame(s = "s2", v = 10), df)
  expect_message(order_by_rank(df, "s", "v"), "Aggregation function")
  out <- order_by_rank(df, "s", "v", fun.aggregate = mean)
  expect_equal(levels(out$s), c("s2", "s3", "s1"))
})


test_that("respects by variable", {
  df$g <- c("b", "a", "b")
  out <- order_by_rank(df, "s", "v", by = "g")
  expect_equal(levels(out$s), c("s2", "s3", "s1"))
})



context("Ordering by prevalence")

df  <- dplyr::data_frame(a = c("a", "a", "b", "c", "b", "a"))
out <- order_by_prevalence(df, "a")

test_that("returns correctly ordered factor", {
  expect_is(out$a, "factor")
  expect_equal(levels(out$a), c("a", "b", "c"))
})

test_that("retains original row order", {
  expect_equal(as.character(out$a), df$a)
})

test_that("respects direction", {
  out <- order_by_prevalence(df, "a", direction = -1)
  expect_equal(levels(out$a), c("c", "b", "a"))
  expect_equal(as.character(out$a), df$a)
})
