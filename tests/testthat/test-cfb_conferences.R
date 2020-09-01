library(testthat)
library(cfbscrapR)

context("CFB Conferences")
test_that("CFB Conferences", {
  x <- cfb_conferences()
  cols <- c("conference_id","name","long_name","abbreviation")
  expect_equal(nrow(x), 34)
  expect_equal(ncol(x), 4)
  expect_s3_class(x, "data.frame")
  expect_error(cfb_conferences('SEC'))
  expect_equal(colnames(x), cols)
  
})