test_that("filter_data returns expected columns", {

  raw_data <- load_data()

  result <- filter_data(raw_data$cases_month)

  expect_equal(
    colnames(result),
    c(
      "Date",
      "Region",
      "Country",
      "Country Code",
      "Suspected Measles",
      "Clinical Measles",
      "Epi Measles",
      "Lab Measles",
      "Total Measles",
      "Discarded",
      "ASCII"
    )
  )
})

test_that("filter_data returns expected columns pt 2", {

  raw_data <- load_data()

  result <- filter_data(raw_data$cases_year)

  expect_equal(
    colnames(result),
    c(
      "Region",
      "Country",
      "Country Code",
      "Year",
      "Total Pop.",
      "Annualized Pop. 2025",
      "Total Suspected Cases",
      "Total Measles",
      "Total Lab Measles",
      "Total Epi Measles",
      "Total Clinical Measles",
      "Total Measles per Million",
      "Total Discarded",
      "Total Discarded per Million"
    )
  )
})

test_that("filter_data returns expected columns pt 2", {
  expect_error(filter_data("poop"))
})



