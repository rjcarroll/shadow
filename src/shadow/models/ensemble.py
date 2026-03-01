"""Stage 1: Ensemble model for predicting third-party intervention.

Replaces R scripts 12-assessClassifiers.R through 16-makePirate.R.

Predicts, for each directed dyad in a country-year with a civil war onset,
whether the potential intervener will:
  - Not intervene (class 0)
  - Intervene for the government (class 1)
  - Intervene for the opposition (class 2)

Pipeline:
  1. PCA on 119+ predictors (retain components to the elbow)
  2. Train component models via 10-fold CV across 25 imputations:
       - Random forest (sklearn RandomForestClassifier)
       - Elastic net (sklearn SGDClassifier or LogisticRegression with L1/L2)
       - Multinomial logit (sklearn LogisticRegression)
       - MLP (sklearn MLPClassifier)
  3. Stack with a non-negative least squares weighting (scipy.optimize.nnls)
     to form the super learner ensemble
  4. Fixed-point iteration to incorporate spatial lag predictors

Performance metric: multinomial log-loss + proportional reduction in loss (PRL)
"""
