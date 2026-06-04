#' Get World Data for Map
#'
#' @returns Cleaned Dataset with Map Polygons for Leaflet Map
#'
#' @importFrom rnaturalearth ne_countries
#' @importFrom dplyr filter mutate case_when rename
#' @export
#'
#' @examples
#' load_WorldData(2016)

load_WorldData <- function(target_year) {

  # validate year
  validateYear(target_year)

  # load measles data
  raw_data <- load_data()

  # world
  world <- rnaturalearth::ne_countries(scale = "medium",
                                       returnclass = "sf")

  # renaming Country names to match world dataset
  measles_renamed <- raw_data |>
    dplyr::filter(Year == target_year) |>
    dplyr::mutate(
      Country = dplyr::case_when(
        Country == "Congo" ~ "Republic of the Congo",
        Country == "Eswatini" ~ "Kingdom of eSwatini",
        Country == "Gambia" ~ "The Gambia",
        Country == "United Republic of Tanzania" ~ "Tanzania",
        Country == "Bolivia (Plurinational State of)" ~ "Bolivia",
        Country == "United States of America" ~ "United States",
        Country == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
        Country == "Iran (Islamic Republic of)" ~ "Iran",
        Country == "Syrian Arab Republic" ~ "Syria",
        Country == "Czechia" ~ "Czech Republic",
        Country == "Netherlands (Kingdom of the)" ~ "Netherlands",
        Country == "Republic of Moldova" ~ "Moldova",
        Country == "Türkiye" ~ "Turkey",
        Country == "United Kingdom of Great Britain and Northern Ireland" ~ "United Kingdom",
        Country == "Democratic People's Republic of Korea" ~ "Dem. Rep. Korea",
        Country == "Lao People's Democratic Republic" ~ "Lao PDR",
        Country == "Micronesia (Federated States of)" ~ "Federated States of Micronesia",
        Country == "Viet Nam" ~ "Vietnam",
        TRUE ~ Country))

  # join measles data to world polygons
  world_data <- dplyr::left_join(world, measles_renamed, by = c("name_long" = "Country"))

  return(world_data)}
