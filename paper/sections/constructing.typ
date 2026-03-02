= A Predictive Measure of Intervention Expectations <sec-constructing>

The shadow of intervention shapes rebel decisions before a single shot is fired.
Operationalising that shadow requires a measure of the _expected_ intervention
environment facing each potential rebel group at the moment it weighs the costs
and benefits of open conflict.  I construct such a measure in two stages.

In the first stage, I train a machine-learning ensemble on all directed dyads
(potential intervener B, civil war host A) observed during the 1946--1999 Regan
period to predict the probability that B sends military forces into any given
civil conflict in A.  The training outcome is coded from @regan2000.
In the second stage, these dyad-level predicted probabilities are aggregated
across all potential interveners to produce a country-year measure of the total
expected intervention environment facing the host country---the "shadow" that
theory predicts should suppress civil war onset.

This section describes the Stage 1 classifier: the training data, the feature
set and its theoretical rationale, and the modelling choices that produce
calibrated probability estimates.

== Outcome Variable

The Stage 1 training labels come from @regan2000, whose dataset records
third-party military, economic, and diplomatic interventions in civil conflicts
from 1944 to 1999.  I restrict attention to _military_ interventions, coded as
_government-biased_ (1) or _opposition-biased_ (2) according to the `target`
variable.  Regan's raw data contain a small number of internally inconsistent
coding decisions; following the corrections documented in Online Appendix A,
I deduplicate to one record per directed dyad-year.

For each directed dyad $(B arrow.r A)$ active in a civil war onset year, the
training label is: 0 (no military intervention), 1 (government-biased), or 2
(opposition-biased).  Non-onset dyad-years are excluded from the training set.
This produces 219 intervention-coded onset observations across 73 host countries
and 67 unique intervening states over the 1946--1999 period.

== Feature Set

The classifier draws on a rich set of directed-dyad-year features organised
around five substantive dimensions.  All continuous variables with missing data
are multiply imputed using miceforest (five country-year imputation datasets
crossed with five undirected-dyad imputation datasets, yielding 25 complete
datasets); details appear in Online Appendix B.  The full variable list with
sources and coverage is in Online Appendix C.

=== State Characteristics of the Potential Intervener

A state's willingness and ability to project military force abroad depends
fundamentally on its own political and material situation.

*Material capabilities* are measured by the Composite Index of National
Capability (CINC) from the COW National Material Capabilities Dataset v6
@singer1987, supplemented by its six component series (military expenditure,
military personnel, iron and steel production, energy consumption, urban
population, and total population).  Log GDP per capita and log population, drawn
from V-Dem v15 @vdem2025, provide additional economic context.

*Regime type* is measured by the Polity II score (`polity2`, $-10$ to $+10$) from
V-Dem's `e_polity2` series, together with a liberal democracy index
(`v2x_polyarchy`) and an instability indicator combining large Polity swings and
coup events.  Polity lags (years $t-1$ through $t-4$) capture the trajectory
of political change.

*Power status* is captured by two binary indicators: `major_power`, coded 1 if
the state holds COW major-power designation in that year (COW Major Powers
Dataset, version 2024); and `is_P5`, coded 1 for the five permanent members of
the UN Security Council (USA, UK, and France throughout our sample period;
USSR/Russia through 1991 and from 1992 respectively; PRC China from 1971 when
the People's Republic displaced the Republic of China at the UN).  Major powers
and P5 members face systemically different intervention incentives---greater
capacity to project force, greater stakes in preserving international order, and
heightened reputational returns to decisive action.

*Foreign policy preferences* are measured by the state's ideal-point estimate
on the liberal--conservative dimension of UN General Assembly voting, from the
Voeten--Strezhnev--Bailey dataset @bailey2017, updated through June 2024.
UNGA ideal points capture revealed foreign policy positions independent of the
specific conflict at hand; more liberal-leaning states have historically been
more likely to intervene on behalf of incumbent governments @regan2000.

*Intervention history* is measured by `recent_int`, the count of distinct
military interventions the state has made in the five years prior to the current
year, derived from the Regan dataset.  Recent interventionism reflects both the
state's political willingness to use force abroad and the institutional capacity
accumulated through prior deployments.

=== Bilateral Relationship

The strategic logic of intervention is inherently relational: a state's
calculus depends on its specific ties to the host country.

*Alliance commitments*---defense pacts, neutrality pledges, non-aggression
treaties, and ententes---come from the COW Formal Alliances Dataset v4.1
@gibler2009.

*Trade interdependence* is measured by bilateral imports from the COW Dyadic
Trade Dataset v4.0.  States with strong commercial relationships face higher
costs from instability in each other's territory.

*Geographic proximity* is captured by log capital distance and a six-level
contiguity indicator from the COW Direct Contiguity Dataset v3.2 @stinnett2002.

*Colonial ties*---whether B was ever A's colonial ruler, or vice versa---come
from the COW Colonial Contiguity Dataset.

*Territorial disputes* are measured by the count and maximum salience of active
claims from the ICOW Territorial Claims Project v10.1 @huth2003.

*Expected dispute outcomes* come from the Carroll--Kenkel Dispute Outcome
Expectations model v2.0 @carroll2019.

*Peace quality* is measured by the Diehl--Goertz--Gallegos Peace Score v2.01
@diehl2019, ranging from active rivalry (0) to stable peace (1).

=== Foreign Policy Alignment

*UN ideal point distance*---the absolute difference between B's and A's UNGA
ideal points---measures the extent to which the two states share foreign policy
preferences.  Ideologically proximate states are better positioned to coordinate
and more likely to favour similar conflict outcomes.

*Shared IGO membership* counts the intergovernmental organisations in which both
states hold full membership, from the COW Dyadic IGO Dataset v3 @pevehouse2020.
Common institutional environments reduce coordination costs.

=== Host Country Characteristics

Civil war onset, ongoing wars, and prior war history come from the COW
Intra-State Wars Dataset v5.1.  The remaining host-country covariates---regime
type, GDP per capita, population, oil wealth, mountainous terrain, ethnic
fractionalization @fearon2003, ethnic exclusion from the EPR Dataset 2021
@cederman2010, and colonial history---are included as predictors of the conflict
environment that shapes external powers' risk assessments.

=== Spatial Context

A potential intervener does not decide in isolation: the behaviour of other
major powers shapes its calculus through signalling, burden-sharing, and
audience-cost dynamics.  I capture this through ten spatial lag variables
constructed from a row-normalised polity-similarity weight matrix $W$, where
$W_(i j) = 1 slash |p_i - p_j|$ (zero on the diagonal and where scores are
equal) and $p$ is the Polity II score.  The spatial lags record the weighted
fraction of other potential interveners currently coded government- or
opposition-biased, direct indicators of US and Soviet intervention posture, and
four superpower interaction terms.  Full construction details appear in Online
Appendix D.

== Classifier Design

The multinomial outcome (0 = no intervention, 1 = government-biased,
2 = opposition-biased) is modelled using a super-learner ensemble.
I first apply principal components analysis to the full feature matrix
and retain components to the scree-plot elbow, reducing the effective
dimensionality while preserving most of the explained variance.

Four component classifiers are trained within a ten-fold cross-validation
scheme: a random forest, an elastic-net penalised multinomial logit, an
unpenalised multinomial logit, and a multi-layer perceptron.  Each
classifier produces a vector of class probabilities for every
training observation.  The super learner combines these using
non-negative least squares (NNLS) weights chosen to minimise held-out
multinomial log-loss, so that the ensemble adaptively up-weights
component classifiers that predict well on out-of-fold observations.

All training takes place within the Regan period (1946--1999), and all
reported performance statistics are based on out-of-fold predictions to
guard against overfitting.  The primary performance metric is the
proportional reduction in log-loss relative to the class-frequency null
model (PRL); the area under the ROC curve (AUC) is also reported.  Because
the null prediction for any given onset dyad-year is "no intervention" for
nearly all observations, even small PRL improvements represent substantial
gains over naive guessing.

*Spatial lags and Nash fixed-point iteration.*  The spatial context variables
described in @appendix-spatial depend on the intervention choices of
_other_ potential interveners---quantities that are themselves to be
predicted.  This creates a concrete measurement problem.  Without an
explicit consistency requirement, the spatial lags fed into prediction
are computed from _observed_ training-data intervention choices, which
are not the model's own outputs.  Applied out-of-sample---to
country-years very unlike the training-period equilibrium---the resulting
predictions embed a deterministic discrepancy between lag inputs and
probability outputs whose sign and magnitude depend on how far the
out-of-sample world departs from training-period behaviour.  This is not
sampling noise; it is a systematic error, and there is no principled
basis for bounding it.  In a measure used precisely to capture
counterfactual intervention environments in pre-conflict years, the bias
could be large.

The fix is to require that the predictions be self-consistent inputs to
themselves:

$ sigma^*(B, A, t) = M(X(B, A, t, sigma^*); hat(theta)) quad forall (B, A, t), $ <eq-fp>

where $M$ is the trained classifier and $hat(theta)$ its estimated
parameters.  At $sigma^*$, the spatial lags are derived from the
model's own equilibrium predictions, not from historical observations.
The deterministic discrepancy disappears by construction.

This self-consistency condition is also the Nash equilibrium condition
for a natural class of games.  The primitive object is the best-response
correspondence, not the game itself: a game specifies a best-response
correspondence _derivatively_, via utility maximisation; $M$ specifies
one _directly_, from the data.  Nash equilibrium is a fixed point of
the best-response correspondence and does not require identifying the
payoff functions that generate it.  Games can be partitioned into
equivalence classes under the relation "generates the same best-response
correspondence"; $sigma^*$ is the Nash equilibrium of every game in the
class whose correspondence $M$ estimates.#footnote[
  The set of rationalising payoff functions for any interior $sigma^*$
  is non-empty---choose utilities that make each player indifferent among
  actions assigned positive probability.  What is identified is the
  correspondence; the payoffs that generate it are not.  A natural
  parametric fibre of these classes is the linear-utility family
  $u_i (a, sigma_(- i)) = alpha dot.c f_i (a) + beta dot.c W_i (a, sigma_(- i))$,
  where $f_i$ captures state-specific intervention incentives and $W_i$
  the spatial interaction term.  The best-response from this family is a
  softmax---the functional form of the multinomial logit component of
  the super-learner---so the parametric case is explicitly represented
  in the ensemble while the broader non-parametric mixture accommodates
  departures from it.
]
If the training data reflect equilibrium play, $M$ is a non-parametric
estimate of the true best-response correspondence $"BR"^*$ and $sigma^*$
estimates its fixed point, without specifying which game in the
equivalence class is the true data-generating process.  The statistical
and game-theoretic justifications therefore coincide: @eq-fp corrects a
concrete measurement error and simultaneously identifies the equilibrium
of the underlying intervention game, whichever member of the equivalence
class that game happens to be.

@eq-fp has no closed form but is solved by fixed-point iteration.
The iteration proceeds in two stages.  In the _training stage_, the
classifier is first fit using observed spatial lags; out-of-fold predictions
then replace the observed intervention indicators to recompute spatial lags;
and the classifier is refit on the updated lags.  This loop repeats until
convergence, typically within two to three passes.  In the _prediction
stage_, the trained classifier is held fixed and only the spatial lags are
updated: the model predicts for every directed dyad-year (onset and non-onset
alike), spatial lags are recomputed from those predictions, and the model
re-predicts until convergence.  Applying the fixed-point universally ensures
that the shadow measure reflects the counterfactual equilibrium that would
obtain in any country-year if a conflict were to start, not only in years
where a civil war was actually observed.  Restricting the iteration to onset
rows would implicitly assume zero strategic interdependence for the vast
majority of the sample, systematically understating the expected intervention
environment in pre-conflict years and breaking the universality of the
equilibrium.

== Aggregating to Country-Year Intervention Expectations

The Stage 1 classifier produces, for each directed dyad $(B arrow.r A)$
in a civil war onset country-year, probability estimates
$hat(sigma)_B (1)$ and $hat(sigma)_B (2)$ for government- and
opposition-biased intervention, respectively.  To reduce noise from
potential interveners to whom the classifier assigns negligible
probability, I apply a cutpoint filter: intervener $B$ contributes to
the aggregate only if $hat(sigma)_B (k) >= tau$ for $k in {1, 2}$.
The threshold $tau$ is treated as a tuning parameter and selected by
out-of-fold performance in the Stage 2 onset models; in practice,
$tau = 0.001$ performs best, excluding only the very least likely
interveners.

The aggregate intervention-expectation variables for host country $A$ in
year $t$ are

$ E_A^G (t) = sum_(B: hat(sigma)_B (1) >= tau) hat(sigma)_B (1), quad
  E_A^O (t) = sum_(B: hat(sigma)_B (2) >= tau) hat(sigma)_B (2). $

Both sums are mechanically larger in later years as the system expands.
I include a year trend in all second-stage regressions to absorb this
secular growth.  The sums are also right-skewed; following
@burbidge1988, I apply the inverse hyperbolic sine (asinh)
transformation before entering them into the onset models.

Because the Stage 1 classifier is trained and evaluated across 25 complete
imputed datasets, I average the predicted probabilities across datasets
before constructing the aggregate variables, propagating imputation
uncertainty without inflating the size of the second-stage estimation
sample.
