test_that("region is valid", {
  tbl <- load_data()
  clean <- filter_data(tbl$cases_year)
  expect_true(validateRegion("AMRO", clean))
  expect_error(validateRegion("apple"))
  expect_error(validateRegion("AMRO", "poop.txt"))
})
