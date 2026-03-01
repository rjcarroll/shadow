"""Build the country-year dataset.

Replaces R scripts 01-addCYvars.R through 05-imputeUD.R.

Input data sources (see data/README.md):
  - UCDP/PRIO Armed Conflict Dataset (v23+) — civil war onset (1946–2023)
  - Polity5 / V-Dem — regime characteristics
  - COW National Material Capabilities (v6) — state capabilities
  - COW Interstate Wars (v4) and MIDs (v4) — ongoing interstate conflict
  - COW National Trade (v4) — total imports/exports
  - COW Major Power Status — major power designation
  - Ellingsen (2000) ethnic/linguistic/religious data

Output: data/interim/country_year.parquet
"""
