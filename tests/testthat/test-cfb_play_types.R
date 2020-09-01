library(testthat)
library(cfbscrapR)


context("CFB Play Types")
test_that("CFB Play Types", {
  x <- cfb_play_types()
  cols <- c("play_type_id","text","abbreviation")
  expect_equal(nrow(x), 46)
  expect_equal(ncol(x), 3)
  expect_s3_class(x, "data.frame")
  expect_equal(colnames(x), cols)
  
})