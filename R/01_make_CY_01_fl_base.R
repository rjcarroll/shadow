# R/01_make_CY_01_fl_base.R

#' Clean and prepare base Fearon & Laitin country-year data
#'
#' Reads `repdata.dta`, standardizes identifiers, harmonizes country names,
#' drops unused variables, and recodes key predictors.
#'
#' @param path_fl_dta Path to original `repdata.dta` file
#'
#' @return A cleaned tibble with standardized COW codes and predictors
#' @export
#'
#' @examples
#' clean_fl_base("data/raw/repdata.dta")
clean_fl_base <- function(path_fl_dta) {
  # Read raw data
  fl <- haven::read_dta(path_fl_dta)

  # Validate required columns
  required_vars <- c("ccode", "year", "cname", "country")
  missing <- setdiff(required_vars, names(fl))
  if (length(missing) > 0) {
    stop("Missing required columns: ", paste(missing, collapse = ", "))
  }

  # Standardize COW codes and create cyear
  fl <- fl %>%
    mutate(
      ccode = makeCC(ccode),
      cyear = makeCY(ccode, year),
      ccode = fixCY(cyear, type = "FL")$fixedCC,
      cyear = fixCY(cyear, type = "FL")$fixedCY
    ) %>%
    filter(!is.na(cyear)) %>%
    arrange(cyear) %>%
    select(cyear, ccode, year, everything()) %>%
    mutate(ccode = factor(ccode, levels = sort(unique(ccode))))

  # Harmonize country names
  fl <- fl %>%
    mutate(
      cname = ifelse(nchar(cname) <= nchar(country), cname, country),
      cname = factor(cname, levels = sort(unique(cname)))
    ) %>%
    select(-country)

  # Drop unused or irrelevant variables
  drop_vars <- c(
    "durest", "pop", "gdptype", "waryrs", "ended", "ethwar", "western",
    "eeurop", "lamerica", "ssafrica", "asia", "nafrme", "anocl", "deml", "mtnest",
    "ef", "sdwars", "sdonset", "colwars", "colonset", "sdwarl", "colwarl",
    "elevdiff", "plural", "second", "numlang", "plurrel", "minrelpc",
    "war", "lgdpenl1"
  )
  fl <- fl %>%
    select(-any_of(drop_vars)) %>%
    select(-starts_with("emp"), -starts_with("cow"))

  # Recode and rename core variables safely
  fl <- fl %>%
    rename_with(~ "lpop_lag", .cols = "lpopl1") %>%
    rename_with(~ "oil", .cols = "Oil") %>%
    rename_with(~ "ongoing_wars", .cols = "wars") %>%
    rename_with(~ "prior_war", .cols = "warl") %>%
    rename_with(~ "ethnic_onset", .cols = "ethonset") %>%
    rename_with(~ "lgdp", .cols = "gdpen") %>%
    rename_with(~ "lgdp_lag", .cols = "gdpenl")

  # Clean and recode onset flags
  fl <- fl %>%
    mutate(
      onset = ifelse(onset == 4, 1, onset),
      ethnic_onset = ifelse(ethnic_onset == 4, 1, onset),
      lpop_lag = ifelse(cmark == 1, NA, lpop_lag),
      lgdp_lag = ifelse(cmark == 1, NA, lgdp_lag)
    ) %>%
    select(-cmark)

  return(fl)
}
