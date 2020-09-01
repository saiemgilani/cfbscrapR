library(testthat)
library(cfbscrapR)


context("CFB Stats Categories")
test_that("CFB Stats Categories", {
  x <- cfb_stats_categories()
  cols <- c("category")
  expect_equal(nrow(x), 38)
  expect_equal(ncol(x), 1)
  expect_s3_class(x, "data.frame")
  expect_equal(colnames(x), cols)
  
})