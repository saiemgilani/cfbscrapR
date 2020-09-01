library(testthat)
library(cfbscrapR)

test_that("CFB Metrics ESPN Win Probability", {
  
  x <- cfb_metrics_espn_wp(game_id = 401012356)
  
  expect_s3_class(x, "data.frame")

})