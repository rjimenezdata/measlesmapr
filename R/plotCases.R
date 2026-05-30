#' Plot number of cases in region(s) by year.
#'
#' @param region A vector of region names.
#' @param legend Whether a legend should be displayed (warning: typically illegible)
#'
#' @returns A plot displaying number of measles cases across all years in these regions.
#' @export
#'
#' @examples
#' plotCases("AMRO")

plotCases <- function(region, legend = F) {

  raw_data <- load_data()

  measles <- filter_data(raw_data$cases_year)

  validateRegion(region, measles)

  max_point <- measles |>
    dplyr::filter(Region %in% region) |>
    dplyr::slice_max(`Total Measles per Million`, n = 1)


  if (legend == F) {

    measles |>
      dplyr::filter(Region %in% region) |>
      ggplot2::ggplot(ggplot2::aes(x = Year, y = `Total Measles per Million`, color = Country)) +
      ggplot2::geom_line() + ggplot2::geom_point() +
      ggplot2::labs(title = paste0("Measles Cases per Million People in ", paste(region, collapse = ", "))) +
      ggplot2::theme_minimal() + ggplot2::theme(legend.position = "none", panel.grid.minor = ggplot2::element_blank()) +
      ggrepel::geom_text_repel(
        data = max_point,
        ggplot2::aes(label = Country),
        show.legend = FALSE,
        nudge_x = -.25
      )

  }

  else {

    measles |>
      dplyr::filter(Region %in% region) |>
      ggplot2::ggplot(ggplot2::aes(x = Year, y = `Total Measles per Million`, color = Country)) +
      ggplot2::geom_line() + ggplot2::geom_point() +
      ggplot2::labs(title = paste0("Measles Cases per Million People in ", paste(region, collapse = ", "))) +
      ggplot2::theme_minimal() + ggplot2::theme(panel.grid.minor = ggplot2::element_blank()) + ggrepel::geom_text_repel(
        data = max_point,
        ggplot2::aes(label = Country),
        show.legend = FALSE,
        nudge_x = -.25
      )

  }


}
