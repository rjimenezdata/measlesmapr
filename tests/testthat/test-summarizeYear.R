test_that("display 2015", {
  expect_s3_class(summarizeYear(2015), "tbl_df")
  expect_equal(names(summarizeYear(2015)), c("Year", "Region", "Total Cases per Million", "Mean Cases per Million", "Most Affected Country", "Maximum Cases per Million"))
  expect_error(summarizeYear(2001))
  expect_error(summarizeYear("apple"))
  tbl <- summarizeYear(2015)
  expect_equal(tbl$Region[1], "AFRO")
  expect_equal(round(tbl$`Total Cases per Million`[2], 2), 7.17)
})


