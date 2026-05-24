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
#' y <- load_data()
#' y

load_data <- function(){
  list(cases_month = readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-24/cases_month.csv'),
       cases_year = readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-24/cases_year.csv'))}
