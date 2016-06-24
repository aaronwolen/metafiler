context('Utilities')

test_that('n_distinct respects grouping', {
  df <- dplyr::data_frame(
    s = rep(letters[1:3], each = 2),
    f = rep(c('f1', 'f2'), 3),
    v = c(1, 2, 2, 1, 4, 3)
  )

  expect_false(is_unique(df, 's'))

  df <- dplyr::group_by(df, f)
  expect_true(is_unique(df, 's'))
})