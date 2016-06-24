context("Profile plot")

test_that("throws message about non-standardized values", {
  df <- dplyr::data_frame(
    s = rep(letters[1:2], each = 2),
    f = rep(c("f1", "f2"), 2),
    v = c(0.4, 0.5, 0.5, 0.5)
  )
  expect_message(plot_profile(df, "s", "f", "v"), "Standardizing values")

  df$v[1] <- 0.5
  expect_silent(plot_profile(df, "s", "f", "v"))
})
