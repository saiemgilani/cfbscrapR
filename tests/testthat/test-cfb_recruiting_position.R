library(testthat)
library(cfbscrapR)

test_that("CFB Recruiting Position Groups", {
  x <- cfb_recruiting_position(2018, team = "Texas")
  y <- cfb_recruiting_position(2016, 2020, team = "Virginia")
  z <- cfb_recruiting_position(2015, 2020, conference = "SEC")
  
  cols <- c("team","conference","position_group","avg_rating",
            "total_rating","commits","avg_stars")
  
  
  expect_equal(colnames(x), cols)
  expect_equal(colnames(y), cols)
  expect_equal(colnames(z), cols)
  
  expect_s3_class(x, "data.frame")
  expect_s3_class(y, "data.frame")
  expect_s3_class(z, "data.frame")
  
})