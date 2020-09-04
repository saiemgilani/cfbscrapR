library(testthat)
library(cfbscrapR)
context("CFB Game Player Stats")
test_that("CFB Game Player Stats", {

  x <- cfb_game_player_stats(2018, week = 15, conference = 'Ind')
  y <- cfb_game_player_stats(2013, week = 1, team = "Florida State", category = 'passing')

  
  expect_s3_class(x, "data.frame")
  expect_s3_class(y, "data.frame")

})