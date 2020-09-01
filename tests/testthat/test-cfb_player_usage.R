library(testthat)
library(cfbscrapR)

context("CFB Player Usage")
test_that("CFB Player Usage", {
  
  x <- cfb_player_usage(year = 2019, position = 'WR', team = 'Florida State')
  
  cols <- c("season","athlete_id", "name", "position", "team", "conference", "usg_overall",
            "usg_pass", "usg_rush", "usg_1st_down", "usg_2nd_down", "usg_3rd_down",
            "usg_standard_downs", "usg_passing_downs")   
  
  expect_s3_class(x, "data.frame")
  expect_equal(colnames(x), cols)
  
  
})