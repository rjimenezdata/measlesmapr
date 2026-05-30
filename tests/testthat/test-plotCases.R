test_that("plot the Americas and Africa", {
  expect_s3_class(plotCases(c("AMRO", "AFRO")), "ggplot")
  expect_s3_class(plotCases("WPRO"), "ggplot")
  expect_error(plotCases("apple"))
})
