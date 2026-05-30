#' Filters Data
#'
#' @param data Dataset to filter.
#'
#' @returns Filtered and cleaned data frame.
#'
#' @export

filter_data <- function(data){

  if ("month" %in% colnames(data)) {

    dplyr::select(-c(rubella_clinical, rubella_epi_linked,
                     rubella_lab_confirmed, rubella_total)) |>
      dplyr::rename("Region" = region,
             "Country" = country,
             "Country Code" = iso3,
             "Year" = year,
             "Month" = month,
             "Suspected Measles" = measles_suspect,
             "Clinical Measles" = measles_clinical,
             "Epi Measles" = measles_epi_linked,
             "Lab Measles" = measles_lab_confirmed,
             "Total Measles" = measles_total,
             "Discarded" = discarded) |>
      dplyr::mutate(Date = my(stringr::str_c(Month, "-", Year)),
             ASCII = stringi::stri_trans_general(Country, "Latin-ASCII")) |>
      dplyr::select(-c(Year, Month)) |>
      dplyr::relocate(Date)

  }

  else {

    dplyr::select(-c(rubella_clinical, rubella_epi_linked,
                     rubella_lab_confirmed, rubella_total,
                     rubella_incidence_rate_per_1000000_total_population)) |>
      dplyr::rename("Region" = region,
             "Country" = country,
             "Country Code" = iso3,
             "Year" = year,
             "Total Pop." = total_population,
             "Annualized Pop. 2025" = annualized_population_most_recent_year_only,
             "Total Suspected Cases" = total_suspected_measles_rubella_cases,
             "Total Measles" = measles_total,
             "Total Clinical Measles" = measles_clinical,
             "Total Epi Measles" = measles_epi_linked,
             "Total Lab Measles" = measles_lab_confirmed,
             "Total Measles" = measles_total,
             "Total Measles per Million" = measles_incidence_rate_per_1000000_total_population,
             "Total Discarded" = discarded_cases,
             "Total Discarded per Million" = discarded_non_measles_rubella_cases_per_100000_total_population)

  }

}
