test_that("mapCases works", {
  # type
  expect_s3_class(mapCases(2017), "leaflet")
  expect_s3_class(mapCases(2019), "leaflet")

  # expect error
  expect_error(mapCases())
  expect_error(mapCases("2019"))
  expect_error(mapCases("100"))
  expect_error(mapCases("3000"))})
