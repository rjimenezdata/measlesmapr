#' Create Line Plot Comparing MCV1 Coverage
#'
#' @param country The country to be analyzed.
#'
#' @param min_year The minimum year for analysis. Note: Earliest data on
#' file is 2012 for most regions except the Americas that start at 2014.
#'
#' @param max_year The maximum year for analysis. Note: Latest data on
#' file is 2025.
#'
#' @returns Generates a line plot displaying a country's MCV1 vaccination rate across time.
#'
#' @export
#'
#' @examples
#' plotVaccination(country = "Romania")

plotVaccination <- function(country, min_year = 2014, max_year = 2024){

  # load data
  raw_data <- load_data()

  # filter data
  measles <- filter_data(raw_data$cases_year)

  # check if years are valid
  validateYear(year1 = min_year,
               year2 = max_year)

  # validate country
  # check type
  if (!is.character(country) || length(country) != 1) {
    stop("country must be a single character string")}

  # check missing/empty
  if (is.na(country) || country == "") {
    stop("country cannot be empty")}

  # create plot
  measles |>
    # select country and years
    dplyr::filter(Country == country,
           Year >= min_year,
           Year <= max_year) |>

    # create plot
    ggplot2::ggplot(aes(x = Year,
               y = `MCV1 Coverage`)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +

    # labels
    ggplot2::labs(paste0(country, ": MCV1 Coverage"),
         x = "Year",
         y = "") +

    # plot theme
    ggplot2::theme_bw() +

    # add percentages to y axis
    ggplot2::scale_y_continuous(labels = \(x) paste0(x, "%"))}
