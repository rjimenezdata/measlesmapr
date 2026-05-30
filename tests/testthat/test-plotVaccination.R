test_that("plotVaccination works", {
  # expect ggplot
  expect_s3_class(plotVaccination(country = "Romania"), "ggplot")
  expect_s3_class(plotVaccination(country = "Chad",
                                  min_year = 2015,
                                  max_year = 2019), "ggplot")

  # expect error
  expect_error(plotVaccination())
  expect_error(plotVaccination(country = "fake"))
  expect_error(plotVaccination(country = "Romania",
                               min_year = 2024,
                               max_year = 2014))
  expect_error(plotVaccination(country = "Romania",
                               min_year = 2010))
  expect_error(plotVaccination(country = "Romania",
                               max_year = 2030))})
