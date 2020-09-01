library(testthat)
library(cfbscrapR)

context("CFB Player Returning")
test_that("CFB Player Returning", {
  
  x <- cfb_player_returning(year = 2020, team = 'Florida State')

  cols <- c("season", "team", "conference", "total_ppa", "total_passing_ppa",
            "total_receiving_ppa", "total_rushing_ppa", "percent_ppa",
            "percent_passing_ppa", "percent_receiving_ppa", "percent_rushing_ppa", "usage",
            "passing_usage", "receiving_usage", "rushing_usage")   
  
  expect_s3_class(x, "data.frame")
  expect_equal(colnames(x), cols)
  
  
})