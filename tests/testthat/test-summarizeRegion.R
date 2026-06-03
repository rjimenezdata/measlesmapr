test_that("summarizeRegion returns wide measles data for a valid region", {
  fake_measles <- tibble::tibble(
    Region = c("Americas", "Americas", "Europe"),
    Year = c(2020, 2021, 2020),
    Country = c("Argentina", "Argentina", "France"),
    `Total Measles per Million` = c(1.2, 2.4, 3.6)
  )

  testthat::local_mocked_bindings(
    load_data = function() fake_measles,
    validateRegion = function(region, measles) TRUE
  )

  result <- summarizeRegion("Americas")

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
  expect_equal(names(result), c("Region", "Country", "2020", "2021"))
  expect_equal(result$Region, "Americas")
  expect_equal(result$Country, "Argentina")
  expect_equal(result$`2020`, 1.2)
  expect_equal(result$`2021`, 2.4)
})

test_that("summarizeRegion sorts year columns", {
  fake_measles <- tibble::tibble(
    Region = c("Americas", "Americas"),
    Year = c(2021, 2020),
    Country = c("Argentina", "Argentina"),
    `Total Measles per Million` = c(2.4, 1.2)
  )

  testthat::local_mocked_bindings(
    load_data = function() fake_measles,
    validateRegion = function(region, measles) TRUE
  )

  result <- summarizeRegion("Americas")

  expect_equal(names(result), c("Region", "Country", "2020", "2021"))
})

test_that("summarizeRegion passes invalid regions to validateRegion", {
  fake_measles <- tibble::tibble(
    Region = "Americas",
    Year = 2020,
    Country = "Argentina",
    `Total Measles per Million` = 1.2
  )

  testthat::local_mocked_bindings(
    load_data = function() fake_measles,
    validateRegion = function(region, measles) {
      stop("Invalid region", call. = FALSE)
    }
  )

  expect_error(
    summarizeRegion("Invalid Region"),
    "Invalid region"
  )
})
