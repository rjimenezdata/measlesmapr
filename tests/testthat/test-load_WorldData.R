test_that("load_WorldData works", {
  res <- load_WorldData(2017)

  # sf type
  expect_s3_class(res, "sf")

  # expect true
  expect_true("Nominal GDP per Capita" %in% names(res))

  # expect error
  expect_error(load_WorldData(target_year = 9999))})
