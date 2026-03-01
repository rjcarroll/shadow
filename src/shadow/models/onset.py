"""Stage 2: Civil war onset models.

Replaces R scripts 17-topNodeFit.R through 23-introducePredictors.R.

Takes the predicted intervention expectations from Stage 1 and tests whether
they improve prediction of civil war onset (country-year binary outcome).

Main models (logistic regression via statsmodels):
  - Baseline: Fearon & Laitin (2003) covariates only
  - Entrants: + expected number of interveners per side
  - Powers: + expected major power intervention
  - Neighbors: + expected intervention by contiguous states
  - Coethnics: + expected intervention by co-ethnic states
  - Rivals: + expected intervention by interstate rivals
  - Rulers: + expected intervention by former colonial rulers
  - Full: all of the above

Robustness checks (sklearn):
  - Random forest classifier
  - Gradient boosting (XGBoost or LightGBM)

Evaluation:
  - In-sample: log-loss, AUC, Vuong test vs. baseline
  - Out-of-sample: 10×10-fold cross-validation, log-loss, AUC
  - Proportional Reduction in Loss (PRL) relative to null model

Output: results/tables/tab-topFit.tex, results/figures/
"""
