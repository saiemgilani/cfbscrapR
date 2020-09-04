context("CFB Game Box Advanced")

x <- cfb_game_box_advanced(game_id = 401012356)

y <- cfb_game_box_advanced(game_id = 401110720)

cols <- c("stat", "team1", "team2")

test_that("CFB Game Box Advanced", {
  expect_equal(nrow(x), 58)
  expect_equal(nrow(y), 58)
  expect_equal(colnames(x), cols)
  expect_equal(colnames(y), cols)
  expect_s3_class(x, "data.frame")
  expect_s3_class(y, "data.frame")
})