#' Patch early 1940s civil war indicators based on prior manual inspection
#'
#' @param CYdata A country-year tibble with `cyear`, `ccode`, `year`,
#'               and war indicators
#'
#' @return Patched tibble with fixed values for Spain and Greece
#' @export
patch_early_civilwars <- function(CYdata) {
  CYdata <- CYdata %>%
    dplyr::mutate(
      ongoing_wars = dplyr::if_else(is.na(ongoing_wars) & year < 1945, 0, ongoing_wars),
      prior_war    = dplyr::if_else(is.na(prior_war)    & year < 1945, 0, prior_war),
      onset        = dplyr::if_else(is.na(onset)        & year < 1945, 0, onset),
      ethnic_onset = dplyr::if_else(is.na(ethnic_onset) & year < 1945, 0, ethnic_onset),
      aim          = dplyr::if_else(is.na(aim)          & year < 1945, NaN, aim),
      casename     = dplyr::if_else(is.na(casename)     & year < 1945, "NA", casename)
    ) %>%
    dplyr::mutate(
      prior_war = dplyr::if_else(cyear == "230_1940", 0, prior_war),
      ongoing_wars = dplyr::case_when(
        cyear == "350_1944" ~ 1,
        cyear == "350_1945" ~ 1,
        TRUE ~ ongoing_wars
      ),
      prior_war = dplyr::if_else(cyear == "350_1945", 1, prior_war)
    )

  return(CYdata)
}
