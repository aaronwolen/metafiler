context("Palettes")

test_that("manual_pal_plus appends .colors", {
  x <- c("red", "blue")
  f <- manual_pal_plus(x)
  expect_equal(f(2), x)
  expect_equal(f(3), c(x, .colors[1]))
})

test_that("manual_pal_plus maintains names", {
  x <- c(r = "red", "blue")
  f <- manual_pal_plus(x)
  expect_equal(f(2), x)
})


test_that("brewer_pal_plus throws an error for non-existent palettes", {
  expect_error(brewer_pal_plus(palette = 36))
  expect_error(brewer_pal_plus(palette = "R"))
})

test_that("brewer_pal_plus appends .colors", {
  x <- scales::brewer_pal(palette = "Set1")(9)
  f <- brewer_pal_plus(palette = "Set1")
  expect_equal(f(9), x)
  expect_equal(f(10), c(x, .colors[1]))
})
