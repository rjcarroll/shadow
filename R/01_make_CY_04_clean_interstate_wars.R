#  R/clean_interstate_wars.R

#' Clean and aggregate Correlates of War Interstate War data
#'
#' @param path_cow Path to Inter-State War CSV file (v4.0)
#'
#' @return A tibble with cyear, number of wars, and war intensity
#' @export
clean_interstate_wars <- function(path_cow) {
  raw <- readr::read_csv(path_cow, na = c("", "NA", "-8", "-9"), show_col_types = FALSE)

  # Manual override for Second Laotian War
  raw <- dplyr::mutate(raw,
                       BatDeath = dplyr::if_else(WarNum == 170 & ccode == 800, 0, BatDeath)
  )

  # Keep relevant columns
  wars_raw <- raw %>%
    dplyr::select(ccode, WarNum, BatDeath, dplyr::contains("Year")) %>%
    dplyr::mutate(
      ccode = makeCC(ccode)
    ) %>%
    dplyr::group_by(WarNum) %>%
    dplyr::mutate(MaxDeath = max(BatDeath, na.rm = TRUE)) %>%
    dplyr::ungroup()

  # Expand war episodes into country-years
  wars_expanded <- purrr::map_dfr(seq_len(nrow(wars_raw)), function(i) {
    row <- wars_raw[i, ]
    years <- row$StartYear1:row$EndYear1
    if (!is.na(row$StartYear2)) {
      years <- c(years, row$StartYear2:row$EndYear2)
    }

    tibble::tibble(
      ccode = row$ccode,
      year = years,
      batDeath = row$BatDeath,
      maxDeath = row$MaxDeath
    ) %>%
      dplyr::mutate(
        cyear = fixCY(makeCY(ccode, year))$fixedCY
      )
  })

  # Collapse to one row per cyear
  summary <- wars_expanded %>%
    dplyr::group_by(cyear) %>%
    dplyr::summarise(
      numWars = dplyr::n(),
      warIntensity_solo = sum(batDeath, na.rm = TRUE),
      warIntensity_max = sum(maxDeath, na.rm = TRUE),
      .groups = "drop"
    )

  return(summary)
}
