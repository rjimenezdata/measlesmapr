test_that("region is valid", {
  tbl <- load_data()
  expect_true(validateRegion("Americas", tbl))
  expect_error(validateRegion("apple"))
  expect_error(validateRegion("Americas", "poop.txt"))
})
