test_that("load data returns a table", {
  expect_type(load_data(), "list")
})

test_that("load data returns a tbl_df", {
  expect_s3_class(load_data(), "tbl_df")
})

test_that("first column is a region", {
  expect_equal(names(load_data())[1], "Region")
})


test_that("input incorrect",{
  expect_error(load_data(apple))
})
