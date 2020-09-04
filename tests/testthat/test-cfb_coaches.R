context("CFB Coaches")

x <- cfb_coaches(first = "Nick", last = "Saban", team = 'alabama')

cols <- c('first_name','last_name','school','year', 'games',
          'wins', 'losses', 'ties', 'preseason_rank', 'postseason_rank')

test_that("CFB Coaches", {
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})