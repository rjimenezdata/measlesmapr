#' Summarizes the cases within a region's countries
#'
#' @param region Region to summarize. Valid regions include "Africa," "Americas," "Eastern Mediterranean," "Europe," "South East Asia," and "Western Pacific"
#'
#' @returns Data frame with summarized Measles case data for a single region
#' @export
#'
#' @examples
#' summarizeRegion("Americas")
summarizeRegion <- function(region) {

  measles <- load_data()

  validateRegion(region, measles)

  measles |>
  dplyr::filter(Region == {{region}}) |>
    dplyr::select(Region,
                  Year,
                  Country,
                  `Total Measles per Million`) |>
    tidyr::pivot_wider(names_from = Year,
                       values_from = `Total Measles per Million`,
                       names_sort = TRUE)
}
