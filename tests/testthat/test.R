#-----------------------------------#
# Test functions with testthat
#------------------------------------#
context("Thorws error correctly")

test_that("check error", {
  throws_error(fars_read ('accident_1.csv.bz2'))
  throws_error(fars_summarize_years(c(1, 2, 3)))
})

