library(testthat)
library(cfbscrapR)


context("CFB Team Talent")
test_that("CFB Team Talent", {
  x <- cfb_team_talent(year = 2019)
  cols <- c("year","school","talent")
  expect_equal(nrow(x), 231)
  expect_equal(ncol(x), 3)
  expect_s3_class(x, "data.frame")
  expect_equal(colnames(x), cols)
  
})