#' Produces NB GLM statistical analysis output for data
#'
#' @param interaction whether to include interaction between Region and other variables
#'
#' @returns Data frame with NB GLM coefficients
#' @export
#'
#' @examples
#' modelCases()

modelCases <- function(interaction = T) {
  model_data <- load_data()

  model_data <- model_data |>
    dplyr::mutate(cPopDensity = scale(`Density`, scale = F),
                  cNomGDP = scale(`Nominal GDP per Capita`, scale = F),
                  cYear = scale(Year, scale = F),
                  cMCV1 = scale(`MCV1 Coverage`, scale = F))

  if(interaction) {
    model_spec <- glmmTMB::glmmTMB(`Total Measles` ~
                                     (1 | Country) + (1 | cYear) + (1 | Country:cYear) +
                                     cPopDensity*Region +
                                     cMCV1*Region,
                                   data = model_data |>
                                     tidyr::drop_na(`Total Measles`), ziformula = ~ 1,
                                   family = glmmTMB::nbinom2())
  } else {
    model_spec <- glmmTMB::glmmTMB(`Total Measles` ~
                                     (1 | Country) + (1 | cYear) + (1 | Country:cYear) +
                                     cPopDensity+ cMCV1 + Region,
                                   data = model_data |>
                                     tidyr::drop_na(`Total Measles`), ziformula = ~ 1,
                                   family = glmmTMB::nbinom2())
  }

  model_spec |>
    broom.mixed::tidy() |>
    dplyr::mutate(
      bh.adj = stats::p.adjust(p.value, method = "BH")
    ) |>
    dplyr::mutate(
      term = stringr::str_replace_all(
        term,
        c(
          "Region" = "",
          "cPopDensity" = "Pop. Density*",
          "cMCV1" = "MCV1 Coverage*",
          "\\(|\\)" = "",
          "sd__" = ""
        )
      ),
      term = dplyr::if_else(!is.na(group), group, term),
      term = stringr::str_replace_all(term, c("cYear" = "Year*"))
    ) |>
    dplyr::select(-c(group, p.value)) |>
    dplyr::rename(
      "Model Term" = term,
      "Estimate" = estimate,
      "Std. Error" = std.error,
      "Z-Statistic" = statistic,
      "BH Adj. p-value" = bh.adj,
      "Component" = component,
      "Effect Type" = effect
    ) |>
    dplyr::mutate(
      `Effect Type` = stringr::str_replace_all(
        `Effect Type`,
        c(
          "fixed" = "Fixed",
          "ran_pars" = "Random Intercept"
        )
      ),
      Component = stringr::str_replace_all(
        Component,
        c(
          "cond" = "Conditional",
          "zi" = "Zero-Inflated"
        )
      )
    ) |>
    gt::gt() |>
    gt::fmt_number(decimals = 3) |>
    gt::tab_header(
      title = gt::md("**Zero-Inflated NB GLM Coefficients**")
    ) |>
    gt::tab_caption(
      "Table 5: Negative Binomial regression results. Response measures total measles cases across a region in a given year. Baseline region is Africa."
    ) |>
    gt::tab_footnote(
      "*Quantitative variables are centered for interpretability."
    )
}
