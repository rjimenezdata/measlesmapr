#' Summarizes a region's countries within the data set
#'
#' @param region Region to summarize
#'
#' @returns Data frame with summarized Measles case data for a single region
#' @export
#'
#' @examples
#' summarizeRegion(AMRO)
summarizeRegion <- function(region) {

  raw_data <- load_data()
  measles <- filter_data(raw_data$cases_year)

  validateRegion(region, measles)

  measles |>
  dplyr::filter(Region == {{region}}) |>
    dplyr::select(Region, Year, Country, `Total Measles per Million`) |>
    tidyr::pivot_wider(names_from = Year, values_from = `Total Measles per Million`)
}
