# _targets.R

library(targets)
library(tarchetypes)
library(tidyverse)

tar_option_set(
  packages = c("tidyverse", "haven", "janitor", "countrycode")
)

tar_source()  # loads functions from R/

list(
  tar_target(
    path_fl_dta,
    "data/raw/repdata.dta",
    format = "file"
  ),
  tar_target(
    fl_base,
    clean_fl_base(path_fl_dta)
  ),
  tar_target(
    fl_base_rds,
    {
      path <- "data/processed/fl_base.rds"
      write_rds(fl_base, path)
      path
    },
    format = "file"
  )
)
