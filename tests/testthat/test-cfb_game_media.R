
library(testthat)
library(cfbscrapR)

test_that("CFB Game Media", {
  x <- cfb_game_media(2019, week = 1, conference = 'ACC')

  y <- cfb_game_media(2018, week = 4, conference = 'Ind')

  
  cols <- c("game_id", "season", "season_type", "week", 
            "home_team", "home_conference", "home_score",
            "away_team", "away_conference", "away_score", 
            "provider","spread", "formatted_spread", "over_under")
  
  expect_s3_class(x, "data.frame")
  expect_s3_class(y, "data.frame")
  
})