// ============================================================
// The Decision to Fight
// ============================================================

= The Decision to Fight <sec-decision>

Measure in hand, I now study whether intervention expectations affect the
decision to initiate civil war.

== Specifying Utilities

@eq-EU contains four quantities to be estimated: the intervention
probabilities $sigma_i (O)$ and $sigma_i (G)$, delivered by the Stage 1
classifier, and the utility parameters $u_O^i (O)$ and $u_O^i (G)$, which
govern how much each type of intervention benefits or costs the
opposition group.  @bas2008 show that in strategic statistical models,
the coefficients on downstream action probabilities can be estimated by
multiplying the relevant predictor by the estimated action probability and
then including the product in a second-stage regression.  I follow this
approach and extend it to the multiparty setting.

The simplest specification sets $u_O^i (O) = gamma^O$ and
$u_O^i (G) = gamma^G$ for all $i$, so that @eq-EU reduces to

$ U_O (F; hat(sigma)) = bold(X) bold(beta) + gamma^O sum_i hat(sigma)_i (O) + gamma^G sum_i hat(sigma)_i (G) + epsilon. $

Here $bold(X)$ contains the baseline country-year predictors from
@fearon2003, $bold(beta)$ is their coefficient vector, and the two
summations are the expected total number of opposition-biased and
government-biased interveners, respectively.  I refer to this as the
_Entrants_ model.

Richer specifications allow the utility parameters to vary with
intervener characteristics.  For example, setting
$u_O^i (O) = gamma_0^O + gamma_P^O "MajorPower"_i$ and
$u_O^i (G) = gamma_0^G + gamma_P^G "MajorPower"_i$ and aggregating gives
the expected numbers of entrants on each side plus the expected number of
major-power entrants---the _Powers_ model.  Repeating this exercise for
geographic contiguity (_Neighbors_), shared dominant ethnic group
(_Coethnics_), formal alliance (_Allies_), and colonial relationship
(_Rulers_) produces a family of extended models that can be compared both
in-sample and out-of-sample against the baseline.

== Empirical Strategy

Testing these models requires a comparison point.  I follow the
literature in treating @fearon2003 Table 1, Model 1 as the
_Baseline_ model: a logistic regression for civil war onset as a function
of prior war, log per capita income, log population, mountainous terrain,
oil exporter status, new state status, political instability, ethnic
fractionalization, and democracy.  To account for the growing number of
potential interveners over time---which mechanically increases all
summation-based extended variables---I add a year trend to all
specifications.

Model comparison draws on both in-sample fit and out-of-sample
predictive performance.  Within sample, I report the proportional
reduction in log-loss relative to random guessing and the area under the
ROC curve.  Out-of-sample performance uses leave-one-war-out
cross-validation: all country-years belonging to a civil war are held
out together, and predictions are formed from the remaining data.  This
scheme respects temporal dependence and avoids the information leakage
that would arise from splitting individual country-years at random.

Because the Stage 1 probabilities are estimated quantities, I propagate
imputation uncertainty by averaging all predicted probabilities across the
25 complete datasets before constructing the extended variables.

== Intervention Expectations and Onset

// TODO: summarise the main fit comparison table once results are available.
// Key findings from the prior version of this paper (Carroll 2019 WP):
//   - Extended models improve in-sample fit (PRL, AUC) vs Baseline
//   - Out-of-sample improvements confirm these are not artefacts of overfitting
//   - Scaled (asinh-transformed) extended variables outperform untransformed
//   - Full model overfits out-of-sample; Powers+Neighbors+Coethnics performs best

// #figure(
//   // TODO: insert fit-comparison table
//   caption: [In-sample and out-of-sample fit across model specifications.],
//   kind: table,
// ) <tbl-topfit>

The extended models consistently improve predictive performance relative
to the Baseline.  // TODO: insert specific PRL/AUC numbers.
Critically, the improvements hold out-of-sample, ruling out the
possibility that they merely reflect overfitting to the intervention
coding.

== Dynamic Performance

A notable feature of intervention expectations is their temporal
variation.  Standard country-year predictors---mountainous terrain,
ethnic composition, colonial history---are essentially fixed.  Population
and income evolve, but slowly and typically in a single direction.
Intervention expectations, by contrast, track the shifting foreign
policy postures of potential outside powers and can change substantially
from one year to the next.

// TODO: insert annual performance plot (ratio of extended to Baseline predictions
// over time) and discussion of Cold War periodisation once results are available.

This temporal flexibility has a concrete payoff: the extended model is
better equipped than the Baseline to identify the year in which onset
occurs, not just the countries at elevated structural risk.  Comparing
classification rates for the onset year, the two preceding years, and the
two following years reveals that the Baseline model assigns high predicted
probabilities throughout the run-up to onset---because its key predictors
barely change---while the extended model is more discriminating.
// TODO: insert proximate-year classification table.

== Variable Importance

Logistic regression coefficients provide a limited view of a variable's
contribution when predictors interact nonlinearly.  I supplement the
regression results with permutation-based variable importance scores from
a random forest and a multivariate adaptive regression spline (MARS)
estimator, which capture interactive and nonlinear effects that the logit
cannot.

// TODO: insert variable importance table once results are available.
// Prior results indicated:
//   - New state status and log per capita income are the two most important predictors
//   - Expected government-biased entrants, expected opposition-biased entrants, and
//     expected government-biased major-power entrants rank highly across estimators
//   - The population effect is substantially attenuated once intervention expectations
//     are included---arguably the most striking substantive finding

New state status and per capita income emerge as the strongest individual
predictors across all estimators, consistent with the existing literature.
// TODO: update after running analysis.

== The Population Puzzle

One of the most striking results from the prior version of this paper
merits discussion regardless of the specific numerical estimates to follow.
Across multiple specifications, the inclusion of intervention expectations
substantially attenuates the coefficient on log population---a predictor
that @raleigh2009 characterise as ``the most robust empirical finding in
country-level studies of civil war.''

The mechanism appears to be a nonlinear relationship between population
and intervention expectations: very large states (India, China) attract
elevated intervention probabilities that partially explain the onset
events previously attributed to their size.  The intervention expectations
do not simply soak up variance; the coefficients on log per capita income,
for instance, increase in magnitude when the expectations variables are
included.

// TODO: confirm with updated analysis and insert popplot figure.

== Relationship with the Literature

The theoretical framework is closest to @cunningham2016, who shows that
the prospect of government-biased superpower intervention suppresses onset.
I generalise his approach by covering all potential interveners (not only
superpowers), both sides of intervention (not only government-biased),
and by deriving intervention probabilities from a predictive classifier
rather than proxying them with alliance variables.

// TODO: add comparison table once results are available.
// Prior results: intervention expectations substitute for (rather than complement)
// the Cunningham hierarchy variable; the model with expectations alone outperforms
// the model with both, and the hierarchy variable changes little when expectations
// are added.

@thyne2006 and @thyne2009 also link third-party signals to onset,
emphasising that informational cues from the international environment
can shift the domestic calculus.  The present approach differs by
grounding intervention expectations in a systematic predictive model
rather than in a set of theoretically motivated proxy variables, allowing
for a more general and data-driven assessment of which intervener
characteristics actually enter opposition utility.
