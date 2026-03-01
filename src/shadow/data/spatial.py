"""Build spatial weights matrix and add spatial lag predictors.

Replaces R scripts 10-makeWpol.R and 11-addSpatial.R.

Constructs a row-normalized spatial weights matrix W where similarity between
states i and j is based on Polity score distance. Used to capture strategic
interdependence in intervention decisions (if similar states are intervening,
that updates the probability that a given state will too).

Fixed-point iteration:
  1. Start with all predicted probabilities = class priors
  2. Compute spatial lags: W @ predicted_probs
  3. Re-run ensemble with spatial lags as additional predictors
  4. Repeat until convergence (max change < 1 percentage point)

Output: data/interim/spatial_weights.npz
"""
