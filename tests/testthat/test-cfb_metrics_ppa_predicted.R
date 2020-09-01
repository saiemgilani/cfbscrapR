library(testthat)
library(cfbscrapR)

test_that("CFB Metrics PPA Predicted", {
  x <- cfb_metrics_ppa_predicted(down = 1, distance = 10)
  y <- cfb_metrics_ppa_predicted(down = 3, distance = 10)
  
  cols <- c("yard_line","predicted_points")
  
  expect_equal(colnames(x), cols)
  expect_equal(colnames(y), cols)
  expect_s3_class(x, "data.frame")
  expect_s3_class(y, "data.frame")
  
})