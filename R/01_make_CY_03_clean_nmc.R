# R/01_make_CY_03_clean_nmc.R

#' Clean and prepare National Material Capabilities (NMC) data
#'
#' @param path_nmc Path to NMC v4.0 CSV file
#' @param fl_polity Output of previous FL + Polity merge (used to get valid years)
#'
#' @return A tibble of cleaned NMC data with harmonized cyear keys
#' @export
clean_nmc <- function(path_nmc, fl_polity) {
  valid_years <- unique(fl_polity$year)

  readr::read_csv(path_nmc, na = c("", "NA", "-9"), show_col_types = FALSE) %>%
    dplyr::filter(year %in% valid_years) %>%
    dplyr::select(-cinc, -version, -stateabb) %>%
    dplyr::mutate(
      ccode = makeCC(ccode),
      cyear = makeCY(ccode, year),
      ccode = fixCY(cyear)$fixedCC,
      cyear = fixCY(cyear)$fixedCY
    ) %>%
    dplyr::filter(!is.na(ccode)) %>%
    dplyr::select(cyear, ccode, year, dplyr::everything())
}
