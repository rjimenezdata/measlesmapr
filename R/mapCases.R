#' Measles Cases Leaflet Map
#'
#' @param year Year
#'
#' @returns Generates interactive leaflet that covers countries' measles cases,
#' MCV1 Coverage and Nominal GDP per Capita for a given year.
#'
#' @importFrom leaflet leaflet colorFactor addProviderTiles setView addPolygons addLegend
#' @importFrom dplyr filter
#' @export
#'
#' @examples
#' mapCases(2015)

mapCases <- function(target_year) {

  # validate year
  validateYear(target_year)

  # load data with desired year
  measles <- load_WorldData(target_year)

  # region colors
  region_colors <- c(
    "Africa" = "#d35e7f",
    "Americas" = "#9fc7b9",
    "Eastern Mediterranean" = "#a999eb",
    "Europe" = "#9bcdef",
    "South-East Asia" = "#ff9d60",
    "Western Pacific" = "#d4a9ce")

  # assign colors
  pal <- leaflet::colorFactor(
    palette = region_colors,
    domain = measles$Region,
    na.color = "transparent")

  # cleaning
  gdp_numeric <- as.numeric(gsub("[$,]", "", measles$`Nominal GDP`))
  gdp_formatted <- ifelse(is.na(gdp_numeric),
                          "No Data", paste0("$", format(gdp_numeric,
                                                        big.mark = ",",
                                                        scientific = FALSE)))

  popup_content <- paste0(
    "<strong>", measles$name, "</strong><br>",
    "Total Measles per Million: ", ifelse(is.na(measles$`Total Measles per Million`),
                                          "No Data", measles$`Total Measles per Million`), "<br>",
    "MCV1 Coverage: ", ifelse(is.na(measles$`MCV1 Coverage`),
                              "No Data", paste0(measles$`MCV1 Coverage`, "%")), "<br>",
    "Nominal GDP per Capita: ", gdp_formatted)

  # leaflet map
  leaflet::leaflet(measles) |>
    leaflet::addProviderTiles("CartoDB.Positron") |>
    leaflet::setView(lng = 50, lat = 20, zoom = 0.7) |>
    leaflet::addPolygons(
      fillColor = ~pal(Region),
      fillOpacity = 0.7,
      color = "black",
      weight = 0.5,
      popup = popup_content,
      label = ~name,
      highlightOptions = leaflet::highlightOptions(
        weight = 2,
        color = "white",
        bringToFront = TRUE)) |>

    # add legends
    leaflet::addLegend(
      position = "bottomright",
      pal = pal,
      values = ~Region[!is.na(Region)],
      title = "Region")}
