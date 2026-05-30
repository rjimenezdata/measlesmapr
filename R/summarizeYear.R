#' Summarizes a years' worth of global Measles case data
#'
#' @param year Year to summarize
#'
#' @returns Data frame with summarized Measles case data for a range of years
#' @export
#'
#' @examples
#' summarizeYear(2015)
summarizeYear <- function(year) {

  validateYear(year1 = year)

  raw_data <- load_data()

  measles <- filter_data(raw_data$cases_year)

  max_cases <- measles |>

    dplyr::filter(Year == year) |>

    dplyr::group_by(Region) |>

    dplyr::slice_max(`Total Measles per Million`, n = 1) |>

    dplyr::ungroup()

  measles |>
    dplyr::group_by(Year, Region) |>
    dplyr::summarize(
      `Total Cases per Million` = sum(`Total Measles per Million`, na.rm = T),
      `Mean Cases per Million` = mean(`Total Measles per Million`, na.rm = T)
              ) |>
    dplyr::ungroup() |>
    dplyr::filter(Year == year) |>
    dplyr::left_join(max_cases, by = dplyr::join_by(Year == Year,
                                      Region == Region)) |>
    dplyr::rename("Most Affected Country" = "Country",
           "Maximum Cases per Million" = "Total Measles per Million")
}
