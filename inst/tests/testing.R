#-----------------------------------#
# Test functions with testthat
#------------------------------------#


test_that("fars_read check error", {
  throws_error(fars_read ('accident_1.csv.bz2'))
})

test_that("fars_summarize_years check error", {
  throws_error(fars_summarize_years(c(1, 2, 3)))
})

