// ============================================================
// Conclusion
// ============================================================

= Conclusion

Civil war does not begin in a vacuum.  Opposition groups weigh the
prospect of outside military assistance against the threat of
international resistance before they decide to fight.  I have argued
that these anticipatory calculations---the shadow of intervention---are
a systematic cause of civil war onset, and I have developed a
theory, a measure, and an empirical test to support this claim.

The theoretical framework models the opposition group's onset decision
as a function of rational expectations over the intervention choices of
up to $n$ potential outside powers.  Under two simplifying assumptions,
the expected utility of fighting reduces to a weighted sum of
intervener-specific probabilities and utilities---a quantity that is both
theoretically interpretable and empirically tractable.  The framework
generalizes prior work by addressing both government- and
opposition-biased intervention, by modeling the full set of potential
state interveners rather than superpowers alone, and by allowing for
strategic interdependence among interveners through a spatial weighting
scheme.

Operationalising this framework required constructing a measure of
intervention expectations that did not yet exist.  The Stage 1 classifier
draws on a rich directed-dyad feature set---state capabilities, regime
type, bilateral relationships, foreign policy alignment, host-country
characteristics, and spatial context---to produce calibrated probability
estimates for each potential intervener in each civil war onset year.
Multiple imputation across 25 complete datasets propagates uncertainty
from both the country-year and undirected-dyad imputation stages through
to the final model estimates.

The onset analysis delivers three principal findings.  First, both sides
of the shadow operate exactly as the theory predicts.  Expected
government-biased intervention enters negatively ($gamma^G = -1.66$):
the prospect of outside support for the incumbent deters onset.
Expected opposition-biased intervention enters positively
($gamma^O = +0.90$): the prospect of outside support for rebels
encourages fighting.  Once standard errors are corrected for
measurement-stage uncertainty following @knoxlucascho2022, neither
coefficient achieves conventional significance individually, but the
directional pattern is consistent across all 25 measurement draws,
nine extended specifications, and a country fixed-effects estimator.  Second, the shadow variables
carry genuine predictive content that survives out-of-sample validation.
Five of the ten specifications achieve positive out-of-sample proportional
reduction in log-loss under leave-one-onset-group-out cross-validation,
with the strongest gains from type-disaggregated specifications---Powers,
Neighbors, and Rivals (cts) all exceed the aggregate Entrants model---and
out-of-sample AUC rises from 0.650 (Baseline) to 0.696 (Neighbors);
the in-sample PRL rises from 10.1% to 12.2% and AUC from 0.771 to
0.793.  Third, controlling
for intervention expectations progressively attenuates the coefficient on
log population---from 0.110 ($p = 0.073$) to 0.018 ($p = 0.81$) in the
Full model---suggesting that a substantial portion of what the literature
has attributed to ``large country'' risk is in fact a consequence of the
richer strategic environment that large countries generate.

The results carry implications beyond the civil war literature.  Many
political decisions are made in the shadow of anticipated multiparty
responses: party platforms are staked out in anticipation of rivals'
reactions; bilateral agreements are struck with an eye to market
responses; leaders calibrate repression partly on expectations about
international condemnation or support.  The two-stage empirical strategy
introduced here---predict the probabilities of downstream actions, then
use those predictions as regressors in a structural model of the
initiating decision---is applicable wherever a strategic choice is made
in anticipation of others' responses.  Within the civil war literature,
the approach addresses a persistent limitation of the macro-correlates
framework: country-level structural risk factors evolve slowly and
cannot explain the timing of onset within already-risky countries.
Intervention expectations shift with the decisions of specific leaders
and governments---year to year, conflict to conflict---providing the
temporal variation that structural factors cannot.

The paper's two-stage design also speaks to a productive debate about the
appropriate role of machine learning in quantitative IR research.  #cite(<beck2000>, form: "prose")
demonstrated that neural networks substantially improve predictive accuracy over
logistic regression for conflict onset; #cite(<demarchi2004>, form: "prose")
responded that parametric approaches remain preferable when the goal is
hypothesis testing rather than prediction.  The present application dissolves
the tension: the scientific question at Stage 1 is explicitly
predictive---whether intervention is foreseeable from publicly observable
characteristics---and the answer is most cleanly given by the method that
predicts best.  Logistic regression is not excluded on philosophical grounds;
it is entered as a candidate learner and down-weighted by the data.  The
inferential question---whether anticipated intervention affects the onset
decision---belongs to @sec-decision, where a parametric logit is the
appropriate tool.  Separating the two functions produces a pipeline that is
simultaneously more powerful than a single parametric model and more
interpretable than a black-box alternative used end-to-end.
#cite(<carroll2019>, form: "prose") illustrate the same contrast geometrically: their
ternary diagram of super-learner predictions for militarized dispute outcomes shows
probability mass spread across the full three-class simplex, while parametric
models constrain predictions to a lower-dimensional manifold.  The implication
for rare-event research is general: a classifier that cannot escape the base rate
contributes noise rather than signal when its predictions are aggregated.
Whether anticipated intervention shapes the decision to fight is a substantive
question; that it can be measured---at scale, across time, and across the full
population of potential outside powers---is the prior claim on which the
answer depends.
