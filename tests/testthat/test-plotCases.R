test_that("plot the Americas and Africa", {
  expect_s3_class(plotCases(c("Americas", "Africa")), "ggplot")
  expect_s3_class(plotCases("Western Pacific"), "ggplot")
  expect_s3_class(plotCases("Western Pacific", legend = F), "ggplot")
  expect_error(plotCases("apple"))
})
