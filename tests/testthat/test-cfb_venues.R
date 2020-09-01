library(testthat)
library(cfbscrapR)


context("CFB Venues")
test_that("CFB Venues", {
  x <- cfb_venues()
  cols <- c("id","name","capacity","grass","city","state",
            "zip","country_code","location","elevation","year_constructed",
            "dome","timezone")
  expect_equal(nrow(x), 334)
  expect_equal(ncol(x), 13)
  expect_s3_class(x, "data.frame")
  expect_equal(colnames(x), cols)
  
})