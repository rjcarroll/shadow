# # R/utils_country_year.R

library(tidyr)
library(dplyr)

#' Pad and factor COW-style country codes
#'
#' @param codeVar Vector of numeric or character COW codes
#' @param type "char" (default) or "factor"
#' @return Padded character or factor
makeCC <- function(codeVar, type = "char") {
  if (!is.character(codeVar)) codeVar <- as.character(codeVar)
  out <- ifelse(is.na(codeVar), NA, sprintf("%03s", codeVar))

  if (type == "char") return(out)
  else return(factor(out, levels = sort(unique(out))))
}

#' Construct country-year ID string
#'
#' @param cc Country code
#' @param year Year
#' @param sep Separator (default "_")
#' @return String like "255_1994"
makeCY <- function(cc, year, sep = "_") {
  if (!is.character(cc)) cc <- as.character(cc)
  cc <- makeCC(cc)
  ifelse(is.na(cc), NA, paste(cc, year, sep = sep))
}

#' Split cyear into ccode and year
#'
#' @param cyear Character vector like "255_1994"
#' @return tibble with ccode and year
#' Split cyear into ccode and year
#'
#' @param cyear Character vector like "255_1994"
#' @return tibble with ccode (character) and year (numeric)
splitCY <- function(cyear, sep = "_", strict = TRUE) {
  if (!is.atomic(cyear)) stop("`splitCY()` expects a character vector, not a data frame or list.")

  df <- tibble(cyear = cyear)

  # Count parts safely
  n_parts <- stringr::str_count(df$cyear %||% "", sep) + 1
  n_parts[is.na(df$cyear)] <- NA_integer_
  is_bad <- !is.na(n_parts) & n_parts != 2

  if (strict && any(is_bad, na.rm = TRUE)) {
    bad <- df$cyear[is_bad]
    stop(
      "Malformed `cyear` values in splitCY():\n",
      paste(head(bad, 5), collapse = ", "),
      if (length(bad) > 5) paste0(" ... [", length(bad), " total]")
    )
  }

  # Clean return: only ccode and year
  df_clean <- df %>%
    dplyr::filter(!is_bad | is.na(n_parts)) %>%
    tidyr::separate(
      cyear, into = c("ccode", "year"),
      sep = sep, remove = TRUE, extra = "merge", fill = "right"
    ) %>%
    dplyr::mutate(
      ccode = as.character(ccode),
      year = as.numeric(year)
    )

  return(df_clean)
}





#' Fix known problematic country-year IDs
#'
#' @param cyear A character vector of "ccode_year" strings
#' @param type Either "COW" or "FL" (default: "COW")
#' @return tibble with fixedCY and fixedCC
fixCY <- function(cyear, type = "COW") {
  split <- splitCY(cyear) %>%
    mutate(ccode = as.character(ccode))

  # Drop bad codes
  drop_codes <- c("031", "053", "054", "055", "056", "057", "058", "060", "080",
                  "115", "212", "221", "223", "232", "331", "338", "395", "402",
                  "403", "411", "511", "581", "591", "694", "781", "835", "935",
                  "940", "946", "955", "970", "983", "986", "987", "990")
  split$ccode[split$ccode %in% drop_codes] <- NA

  # Conditional corrections
  if (type == "COW") {
    split$ccode[split$ccode %in% c("255", "260", "265") &
                  split$year %in% 1945:1948] <- "255"
    split$ccode[split$ccode %in% c("255", "260") & split$year == 1954] <- NA
    split$ccode[split$ccode == "255" & split$year == 1990] <- NA
  } else if (type == "FL") {
    split$ccode[split$ccode %in% c("255", "260", "265") &
                  split$year < 1949] <- NA
    split$ccode[split$ccode == "260" & split$year >= 1991] <- "255"
  }

  # More custom fixes
  split$ccode[split$ccode == "305" & split$year %in% 1945:1954] <- NA
  split$ccode[split$ccode == "652" & split$year %in% 1959:1960] <- NA
  split$ccode[split$ccode == "740" & split$year %in% 1946:1951] <- NA
  split$ccode[split$ccode == "760" & split$year <= 1970] <- NA
  split$ccode[split$ccode == "771" & split$year == 1971] <- NA
  split$ccode[split$ccode == "051" & split$year %in% 1945:1961] <- NA
  split$ccode[split$ccode == "365" & split$year %in% 1940:1991] <- "364"
  split$ccode[split$ccode == "770" & split$year <= 1971] <- "769"
  split$ccode[split$ccode == "816" & split$year >= 1977] <- "818"
  split$ccode[split$ccode == "345" & split$year >= 1992] <- "347"
  split$ccode[split$ccode == "529" & split$year == 1993] <- NA
  split$ccode[split$ccode == "530" & split$year >= 1993] <- "529"
  split$ccode[split$ccode == "698" & split$year <= 1970] <- NA

  tibble(
    cyear = cyear,
    fixedCY = makeCY(split$ccode, split$year),
    fixedCC = as.character(split$ccode),
    year = split$year
  )
}
