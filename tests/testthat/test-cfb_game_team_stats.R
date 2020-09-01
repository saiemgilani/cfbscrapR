library(testthat)
library(cfbscrapR)

test_that("CFB Game Team Stats", {
  x <- cfb_game_team_stats(year = 2018, week = 9, team = 'Notre Dame')
  y <- cfb_game_team_stats(2013, week = 1, team = "Florida State")
  
  
  # expect_equal(colnames(x), cols)
  # expect_equal(colnames(y), cols)
  expect_s3_class(x, "data.frame")
  expect_s3_class(y, "data.frame")
  
})