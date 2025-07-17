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
  ),
  tar_target(
    path_polity_csv,
    "data/raw/p4v2014.csv",
    format = "file"
  ),
  tar_target(
    fl_polity,
    clean_merge_polity(fl_base, path_polity_csv)
  ),
  tar_target(
    fl_polity_rds,
    {
      path <- "data/processed/fl_polity.rds"
      write_rds(fl_polity, path)
      path
    },
    format = "file"
  ),
  tar_target(
    path_nmc_csv,
    "data/raw/NMC_v4_0.csv",
    format = "file"
  ),
  tar_target(
    fl_nmc,
    clean_nmc(path_nmc_csv, fl_polity)
  ),
  tar_target(
    fl_nmc_rds,
    {
      out_path <- "data/processed/fl_nmc.rds"
      readr::write_rds(fl_nmc, out_path)
      out_path
    },
    format = "file"
  ),
  tar_target(
    path_cow,
    "data/raw/Inter-StateWarData_v4.0.csv",
    format = "file"
  ),
  tar_target(
    interstate_wars,
    clean_interstate_wars(path_cow)
  ),
  tar_target(
    interstate_wars_rds,
    {
      out_path <- "data/processed/interstate_wars.rds"
      readr::write_rds(interstate_wars, out_path)
      out_path
    },
    format = "file"
  ),
  tar_target(
    path_midb,
    "data/raw/MIDB_4.01.csv",
    format = "file"
  ),
  tar_target(
    mids,
    clean_mids(path_midb)
  ),
  tar_target(
    mids_rds,
    {
      out_path <- "data/processed/mids.rds"
      readr::write_rds(mids, out_path)
      out_path
    },
    format = "file"
  )
)
