library(testthat)
library(cfbscrapR)

test_that("CFB Metrics API Pre-Game Win Probability", {
  x <- cfb_metrics_wp_pregame(year = 2019, week = 9, team = 'Texas A&M')
  y <- cfb_metrics_wp_pregame(year = 2017, week = 8, team = 'TCU')
  cols <- c("season", "season_type", "week", "game_id", 
            "home_team", "away_team", "spread", "home_wp")
  
  
  expect_equal(colnames(x), cols)
  expect_equal(colnames(y), cols)
  expect_s3_class(x, "data.frame")
  expect_s3_class(y, "data.frame")
  
})