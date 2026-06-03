#' Loading in Measles Data from TidyTuesday
#'
#' @returns A list containing two dataframes that record measles incidence at different
#' levels of time. The first, cases_month, contains monthly data at the country-year-month
#' level (i.e. Algeria, January 2012). The second, cases_year, contains yearly data
#' at the country-year level (i.e. Algeria, 2012). Together, these datasets include
#' information for 194 countries across multiple years and months.
#'
#' @export
#'
#' @examples
#' load_data()

load_data <- function(){
  path <- system.file("extdata", "measles_df_clean.csv", package = "MeaslesMapR")
  arrow::read_csv_arrow(path) |>
    dplyr::mutate(
      `Nominal GDP` = readr::parse_number(as.character(`Nominal GDP`)),
      `Total Pop.` = readr::parse_number(as.character(`Total Pop.`)),
      `Nominal GDP per Capita` = `Nominal GDP` / `Total Pop.`
    )
}
