"""Build the directed-dyad dataset.

Replaces R scripts 06-makeDDdata.R through 09-trimDD.R.

Expands country-year data to directed dyads (potential intervener → host state)
for all country-years with a civil war onset. Adds dyadic covariates:
  - COW Alliances (ATOP) — alliance ties
  - COW Contiguity — geographic adjacency
  - Hewitt (2005) rivalry data — interstate rivals
  - Trade flows (directed: exports from host to potential intervener)
  - Colonial history — former ruler / colony ties
  - Shared ethnicity — first ethnic group overlap

Output: data/interim/dyad_{imputation}.parquet (one per imputation)
"""
