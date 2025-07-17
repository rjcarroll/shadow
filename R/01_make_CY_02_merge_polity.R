# R/01_make_CY_02_merge_polity.R

#' Merge and clean Polity IV data with base country-year frame
#'
#' Reads Polity data, cleans and lags polity scores, constructs a regime instability
#' variable, imputes justifiable values, and merges with cleaned FL data.
#'
#' @param fl_base Cleaned FL base tibble
#' @param path_polity Path to Polity CSV file (e.g. "p4v2014.csv")
#'
#' @return A tibble of country-year data with Polity and instability variables merged in
#' @export
clean_merge_polity <- function(fl_base, path_polity) {
  polity <- readr::read_csv(path_polity, show_col_types = FALSE) %>%
    dplyr::filter(year %in% 1940:1999) %>%
    dplyr::mutate(cyear = makeCY(ccode, year))

  # Apply fixCY() and join in fixed codes
  polity <-
    polity %>%
    dplyr::left_join(
      fixCY(polity$cyear) %>% dplyr::select(cyear, fixedCY, fixedCC),
      by = c("cyear")
    ) %>%
    dplyr::rename(cyear_raw = cyear, ccode_raw = ccode) %>%
    dplyr::rename(cyear = fixedCY, ccode = fixedCC) %>%
    dplyr::filter(!is.na(ccode)) %>%
    dplyr::select(-cyear_raw, -ccode_raw) %>%
    dplyr::select(cyear, ccode, year, dplyr::everything())

  # Reduce to relevant variables
  polity <- polity %>%
    dplyr::select(-ccode, -year) %>%
    dplyr::select(-flag, -fragment, -post, -change, -d4, -sf, -regtrans,
                  -contains("month"), -contains("day"),
                  -bprec, -interim, -prior, -eprec, -eyear, -byear)

  # Construct authority type
  polity <- polity %>%
    dplyr::mutate(
      authority = dplyr::case_when(
        democ == -66 ~ "interruption",
        democ == -77 ~ "interregnum",
        democ == -88 ~ "transition",
        TRUE ~ "normal"
      ),
      authority = factor(authority, levels = c("normal", "transition", "interregnum", "interruption"))
    ) %>%
    dplyr::select(cyear, polity2, authority)

  # Lag polity2 values
  polity_lagged <- polity %>%
    dplyr::bind_cols(splitCY(polity$cyear)) %>%
    dplyr::group_by(ccode) %>%
    dplyr::arrange(year) %>%
    dplyr::mutate(
      polity2_lag1 = dplyr::lag(polity2, 1),
      polity2_lag2 = dplyr::lag(polity2, 2),
      polity2_lag3 = dplyr::lag(polity2, 3),
      polity2_lag4 = dplyr::lag(polity2, 4)
    ) %>%
    dplyr::ungroup()

  # Create instability indicator
  polity_instab <- polity_lagged %>%
    dplyr::mutate(
      rawInst = dplyr::case_when(
        authority %in% c("transition", "interregnum") ~ "instability",
        abs(polity2 - polity2_lag1) >= 3 |
          abs(polity2_lag1 - polity2_lag2) >= 3 |
          abs(polity2_lag2 - polity2_lag3) >= 3 |
          abs(polity2_lag3 - polity2_lag4) >= 3 ~ "instability",
        TRUE ~ "stability"
      ),
      rawInst = factor(rawInst, levels = c("stability", "instability")),
      rawInst_lag = dplyr::lag(rawInst)
    ) %>%
    dplyr::select(cyear, polity2, authority, rawInst, rawInst_lag)


  # Final merge
  out <- fl_base %>%
    dplyr::select(-any_of(c("polity2", "polity2l", "instab"))) %>%
    dplyr::left_join(polity_instab, by = "cyear") %>%
    dplyr::mutate(
      instab     = rawInst,
      instab_lag = rawInst_lag
    ) %>%
    dplyr::select(-rawInst, -rawInst_lag)
}
