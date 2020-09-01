
library(testthat)
library(cfbscrapR)

test_that("CFB Game Info", {
  x <- cfb_game_info(2019, week = 1, conference = 'ACC')

  y <- cfb_game_info(2018, week = 4, conference = 'Ind')
  
  
  cols <- c("id", "season", "week", "season_type", "start_date", 
            "start_time_tbd", "neutral_site", "conference_game", 
            "attendance", "venue_id", "venue", 
            "home_id", "home_team", "home_conference", 
            "home_points", "home_post_win_prob", 
            "away_id", "away_team","away_conference",
            "away_points", "away_post_win_prob", "excitement_index")  
  
  expect_s3_class(x, "data.frame")
  expect_s3_class(y, "data.frame")
  expect_equal(colnames(x), cols)
  expect_equal(colnames(y), cols)
  
})