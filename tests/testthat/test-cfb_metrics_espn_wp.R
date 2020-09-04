library(testthat)
library(cfbscrapR)
context("CFB Metrics ESPN Win Probability")
test_that("CFB Metrics ESPN Win Probability", {
  
  x <- cfb_metrics_espn_wp(game_id = 401012356)
  
  cols <- c('espn_game_id', 'play_id', 'seconds_left', 
            'home_win_percentage', 'away_win_percentage')
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")

})