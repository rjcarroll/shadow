"""Multiple imputation for missing data.

Replaces R scripts 02-imputeCY.R and 05-imputeUD.R (which used Amelia II).

Strategy:
  - Country-year level: impute with miceforest (MICE with LightGBM)
  - Dyad level: impute directed-dyad variables separately
  - 25 total imputed datasets (5 CY × 5 dyad, as in the original)

Inverse hyperbolic sine transformation applied to right-skewed variables
with structural zeros before imputation, as in the original.

Output: data/interim/imputed_{n}.parquet for n in 1..25
"""
