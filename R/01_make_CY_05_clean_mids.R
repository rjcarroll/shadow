#' Clean and aggregate Correlates of War Militarized Interstate Disputes (MIDs)
#'
#' @param path_mid Path to MIDs CSV file (e.g. "MIDB_4.01.csv")
#'
#' @return A tibble with cyear, number of MIDs and average hostility
#' @export
clean_mids <- function(path_mid) {
  raw <- readr::read_csv(path_mid, na = c("", "NA", "-9"), show_col_types = FALSE)

  # Filter to relevant non-war disputes (fatality < 6)
  mids_raw <- raw %>%
    dplyr::filter(StYear <= 1999, EndYear >= 1945, Fatality < 6) %>%
    dplyr::select(ccode, StYear, EndYear, DispNum3, HostLev) %>%
    dplyr::mutate(
      ccode = makeCC(ccode)
    ) %>%
    dplyr::group_by(DispNum3) %>%
    dplyr::mutate(maxHost = max(HostLev, na.rm = TRUE)) %>%
    dplyr::ungroup()

  # Expand to all relevant country-years
  mids_expanded <- purrr::map_dfr(seq_len(nrow(mids_raw)), function(i) {
    row <- mids_raw[i, ]
    tibble::tibble(
      ccode = row$ccode,
      year = row$StYear:row$EndYear,
      HostLev = row$HostLev,
      maxHost = row$maxHost
    ) %>%
      dplyr::mutate(
        cyear = fixCY(makeCY(ccode, year))$fixedCY
      )
  })

  # Collapse to one row per cyear
  mids_summary <- mids_expanded %>%
    dplyr::group_by(cyear) %>%
    dplyr::summarise(
      numMIDs = dplyr::n(),
      midIntensity_solo = mean(HostLev, na.rm = TRUE),
      midIntensity_max  = mean(maxHost, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::filter(!is.na(cyear))

  return(mids_summary)
}
