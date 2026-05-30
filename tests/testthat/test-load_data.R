test_that("load data returns a list of tables", {
  expect_type(load_data(), "list")
})

test_that("2 tables in the list", {
  expect_named(load_data(), c("cases_month", "cases_year"))
})

test_that("input incorrect",{
  expect_error(load_data(apple))
})
