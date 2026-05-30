#' Summarizes a years' worth of global Measles case data
#'
#' @param year Year to summarize
#'
#' @returns Data frame with summarized Measles case data for a range of years
#' @export
#'
#' @examples
summarizeYear <- function(year) {

  validateYear(year1 = year)

  raw_data <- load_data()

  measles <- filter_data(raw_data$cases_year)

  max_cases <- measles |>

    filter(Year == year) |>

    group_by(Region) |>

    slice_max(`Total Measles per Million`, n = 1) |>

    ungroup()

  measles |>
    group_by(Year, Region) |>
    summarize(
      `Total Cases per Million` = sum(`Total Measles per Million`, na.rm = T),
      `Mean Cases per Million` = mean(`Total Measles per Million`, na.rm = T)
              ) |>
    ungroup() |>
    filter(Year == year) |>
    left_join(max_cases, by = join_by(Year == Year,
                                      Region == Region)) |>
    rename("Most Affected Country" = "Country",
           "Maximum Cases per Million" = "Total Measles per Million")
}
