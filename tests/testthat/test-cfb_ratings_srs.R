library(testthat)
library(cfbscrapR)

test_that("CFB Conference Ratings - Bill C.'s SP+", {
  x <- cfb_ratings_srs(year = 2019)
  y <- cfb_ratings_srs(year = 2012, conference = 'SEC')
  z <- cfb_ratings_srs(year = 2016, conference = 'ACC')
  
  cols <- c('year','team', 'conference', 'division', 'rating')
  
  
  
  
  expect_equal(colnames(x), cols)
  expect_equal(colnames(y), cols)
  expect_equal(colnames(z), cols)
  
  expect_s3_class(x, "data.frame")
  expect_s3_class(y, "data.frame")
  expect_s3_class(z, "data.frame")
  
  
})