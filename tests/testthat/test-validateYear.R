test_that("validateYear works", {

  # pair tests
  # expect error
  expect_error(validateYear("2015", 2020))
  expect_error(validateYear(2015, "2020"))
  expect_error(validateYear(2020, 2015))
  expect_error(validateYear(2010, 2015))
  expect_error(validateYear(2015, 2030))

  # expect warning
  expect_warning(validateYear(2018, 2018))

  # expect true
  expect_true(validateYear(2015, 2020))

  # non-pair tests
  # expect error
  expect_error(validateYear("2015"))
  expect_error(validateYear(2030))

  # expect true
  expect_true(invisible(validateYear(2015)))})
