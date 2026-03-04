#import "../style.typ": booktab

= A Learned Proxy for the Shadow of Intervention <sec-constructing>

The shadow of intervention shapes rebel decisions before a single shot is fired.
Operationalising that shadow requires a measure of the expected intervention
environment at the moment a potential rebel group weighs the costs and benefits
of open conflict.  I construct such a measure in two stages.

- *Stage 1 --- Directed-dyad classification.*
  A super-learner ensemble trained on the Regan dataset of third-party
  interventions predicts the probability that any given state sends military
  forces into another state's civil conflict, conditional on material
  capabilities, bilateral ties, foreign policy alignment, and the spatial
  pattern of other states' choices.
- *Stage 2 --- Country-year aggregation.*
  Dyad-level probabilities are summed across all potential interveners to 
  generate the shadow.  The aggregation imposes a self-consistency
  condition requiring equilibrium strategies to be mutually coherent, so
  the measure reflects a counterfactual equilibrium in any country-year,
  not only those where conflict was observed.

This section describes Stage 1: the training data, the feature set, and the
modeling choices that produce calibrated probability estimates.

== Outcome Variable

The Stage 1 training labels draw primarily from #cite(<regan2000>, form: "prose"),
whose dataset records third-party military, economic, and diplomatic
interventions in civil conflicts from 1944 to 1999.  I extend Regan's coding
through 2014 by hand-coding unambiguous military interventions in post-1999 COW
onset country-years.  The coding threshold requires deployment of military
personnel---troops, combat advisors, or directed proxy forces attributable to a
specific state---on behalf of one side; arms transfers, economic aid, and
sanctuary alone are insufficient.  UN peacekeeping missions are excluded.  The
post-1999 extension adds 31 directed dyad-year records across 14 host countries,
yielding a combined intervention table of 455 records (1944--2014); the full
coding with source notes appears in Online Appendix A.

The civil war universe within which interventions are coded
is drawn from the Correlates of War Intra-State War Dataset (v5.1) @dixon2016,
restricting to wars fought for central control (type~4) or local/secessionist
issues (type~5).  The COW threshold (≥1,000 battle deaths) is theoretically
appropriate: the shadow-of-intervention mechanism requires rebels organized enough
to form rational expectations and conflicts large enough that outside military
involvement is a realistic prospect.  At lower thresholds---such as the
UCDP/PRIO ≥25-death criterion---third-party military intervention becomes
vanishingly rare, swamping any predictive signal.  This yields 153 distinct onset
country-years between 1946 and 1999 and 191 through 2014, the extent of the COW
series.
I restrict attention to _military_ interventions, coded as
_government-biased_ (1) or _opposition-biased_ (2) according to the `target`
variable.  Regan's raw data contain a small number of internally inconsistent
coding decisions; following the corrections documented in Online Appendix A,
I deduplicate to one record per directed dyad-year.

For each directed dyad $(B arrow.r A)$ active in a civil war onset year, the
training label is: 0 (no military intervention), 1 (government-biased), or 2
(opposition-biased).  Non-onset dyad-years are excluded from the training set.
This produces 254 intervention-coded onset observations---150 government-biased
and 104 opposition-biased---across 51 host countries and 59 unique intervening
states.  The five permanent Security Council members
account for 110 of these events (43~percent); the remaining
states---Cold War proxies, neighbors, regional patrons, and coethnics---account
for the majority (@tab-interveners).
This in itself justifies the need for a comprehensive measure of the expected intervention environment: while the powerful states loom large, they are not the whole story, and the shadow includes a wide range of other actors whose behavior is not well captured by existing measures of patron-client ties or great power rivalry.

Training exclusively on onset country-years is not a convenience — it is
the only option.  Intervention in a civil war is undefined absent a civil war;
in non-onset years the outcome is not "zero intervention" but rather
unobserved.  The consequence is that the training sample is selected on
precisely the outcome that the Stage 2 analysis tries to explain: if the
shadow deters onset, then high-shadow country-years are systematically
underrepresented among onsets, and the classifier learns the
feature--intervention mapping from an endogenously filtered sample.  There
is no design that avoids this.  The defense rests on what the classifier
is asked to learn.  The training target is the conditional relationship
between _dyad-level_ features --- alliance portfolios, capability ratios,
colonial ties, geographic proximity, regime similarity --- and the direction
of military intervention, given that a war has started.  These bilateral
characteristics are plausible determinants of how state $B$ behaves toward
state $A$ in a conflict regardless of what brought that conflict about.  A
France--Ivory Coast dyad with a colonial tie, a defense agreement, and a
large capability gap predicts French government-biased intervention whether
the onset was triggered by a coup, an ethnic mobilization, or a rebel group
emboldened by anticipated outside support.  The fixed-point iteration then
handles the extrapolation: it asks what the equilibrium intervention
environment _would_ look like in any country-year, including those where
onset was never observed, without requiring that the training sample be
representative of the full population.

#figure(
  booktab(
    columns: (2fr, 1fr, 1fr, 1fr),
    headers: ("State", "Gov.", "Opp.", "Total"),
    rows: (
      ([United States#super[\u{2020}]], [64], [29], [93]),
      ([USSR / Russia#super[\u{2020}]],  [34], [8],  [42]),
      ([France#super[\u{2020}]],         [23], [5],  [28]),
      ([United Kingdom#super[\u{2020}]], [19], [4],  [23]),
      ([Libya],                           [2],  [12], [14]),
      ([China#super[\u{2020}]],          [3],  [10], [13]),
      ([Cuba],                            [11], [1],  [12]),
      ([South Africa],                    [1],  [11], [12]),
      ([Iran],                            [5],  [6],  [11]),
      ([Ethiopia],                        [4],  [5],  [9]),
      ([Egypt],                           [9],  [0],  [9]),
    ),
  ),
  caption: [Most frequent military interveners (1946--2014); states with
    fewer than nine interventions omitted.
    #super[\u{2020}]~Permanent member of the UN Security Council (P5).
    Government-biased (Gov.) and opposition-biased (Opp.) interventions
    are coded from the Regan `target` variable and post-1999 extension.],
) <tab-interveners>

== Feature Set

The classifier draws on 105 directed-dyad-year features organized
around four substantive dimensions, listed in full with sources in Online
Appendix C.  Many variables enter twice---once for the potential intervener and
once for the host---so that the classifier can learn asymmetric effects.
All continuous variables with missing data are multiply imputed
using miceforest (five country-year imputation datasets crossed with five
undirected-dyad imputation datasets, yielding 25 complete datasets); details
appear in Online Appendix B.

- *State characteristics of both the potential intervener and the host.*
  Material capabilities (CINC scores and components, GDP, population) from COW
  v6 @singer1987 and V-Dem v15 @vdem2025; regime type (Polity~II and lags,
  liberal democracy index, instability); power status (COW major power, P5
  membership); and host-side conflict history, ethnic fractionalization
  @fearon2003, ethnic exclusion @cederman2010, oil wealth, and terrain.

- *Bilateral ties.*  Alliance commitments @gibler2009, bilateral trade,
  geographic proximity (log capital distance, contiguity @stinnett2002),
  colonial history, territorial disputes @huth2003, expected dispute outcomes
  @carroll2019, and peace quality @diehl2019.  Contiguous states face
  qualitatively distinct incentives: beyond capability projection, they
  intervene primarily to contain spillover @kathman2010.

- *Foreign policy alignment.*  UN General Assembly ideal points
  @bailey2017 for each state and the absolute distance between them;
  shared IGO membership from COW Dyadic IGO v3 @pevehouse2020.

- *Spatial context.*  Ten spatial lag variables constructed from a
  row-normalised polity-similarity weight matrix capture the weighted behavior
  of other potential interveners in the same host-year: the fraction
  coded government- or opposition-biased, direct indicators of US and
  Soviet intervention posture, and four superpower interaction terms
  @gent2007 @salehyan2007.  Full construction details appear in Online
  Appendix D.

Before training, the 105 features are standardised and reduced via principal
components analysis, retaining components to 90% of cumulative variance
(54~components across all 25 imputation datasets).

== Classifier Design

The multinomial outcome (0 = no intervention, 1 = government-biased,
2 = opposition-biased) is modeled using a super-learner ensemble
@vanderlaan2007.

Nine component classifiers spanning three model families are trained
within a ten-fold cross-validation scheme: two tree ensembles (a random
forest @breiman2001 and two histogram-based gradient boosting
specifications at different learning rates), four logistic regression variants (ridge,
elastic-net, lasso, and unpenalised multinomial), and two multi-layer
perceptrons (shallow and deeper architectures).  The unpenalised
multinomial logistic regression---a softmax best-response function---directly
nests the functional-form assumption of structural game-theoretic models of
intervention @gibilisco2022; its stacking weight is determined by
out-of-fold predictive performance, so whether that assumption fits the data
is answered empirically rather than imposed by construction.  Each
classifier produces a vector of class probabilities for every
training observation.  The super learner combines these using
non-negative least squares (NNLS) weights chosen to minimize held-out
multinomial log-loss, so that the ensemble adaptively up-weights
component classifiers that predict well on out-of-fold observations.

All training takes place within the Regan period (1946--1999), and all
reported performance statistics are based on out-of-fold predictions to
guard against overfitting---following #cite(<wang2018>, form: "prose")'s demonstration that
evaluation of class-imbalanced political-event models on training data
produces severely inflated performance estimates @muchlinski2016.
The primary performance metric is the proportional reduction in log-loss
relative to the class-frequency null model (PRL); the area under the ROC
curve (AUC) is also reported.  Because the null prediction for any given
onset dyad-year is "no intervention" for nearly all observations, even
small PRL improvements represent substantial gains over naive guessing.

*Spatial lags and Nash fixed-point iteration.*  The broader motivation for
the two-stage design is that intervention decisions are endogenous to the
conflict environment: interveners select into conflicts strategically, and
a single-equation model that treats realized intervention as a covariate in
an onset regression conflates the causal effect with the selection process
that generated it @signorino2002 @gent2008.  Qualitative evidence confirms
the problem directly: rebel groups in Bosnia and Kosovo explicitly
conditioned their onset decisions on anticipated outside support, so
realized intervention is partly a consequence of expected intervention
rather than an independent cause @kuperman2008.  Using predicted
intervention expectations rather than realized intervention is the
appropriate correction.

The spatial context variables described in @appendix-spatial depend on the
intervention choices of _other_ potential interveners---quantities that are
themselves to be predicted.  A documented pattern of countervailing
intervention makes these dependencies quantitatively important: when one
state intervenes on the government side, ideologically rival states face a
selective incentive to support the opposition, and vice versa @salehyan2011
@findleyteo2006.  This creates a concrete measurement problem.
Without an explicit consistency requirement, the spatial lags fed into
prediction are computed from _observed_ training-data intervention choices,
which are not the model's own outputs.  Applied out-of-sample---to
country-years very unlike the training-period equilibrium---the resulting
predictions embed a deterministic discrepancy between lag inputs and
probability outputs whose sign and magnitude depend on how far the
out-of-sample world departs from training-period behavior.  This is not
sampling noise; it is a systematic error, and there is no principled
basis for bounding it.  In a measure used precisely to capture
counterfactual intervention environments in pre-conflict years, the bias
could be large.

The fix is to require that the predictions be self-consistent inputs to
themselves:

$ sigma^*(B, A, t) = hat(M)(X(B, A, t, sigma^*)) quad forall (B, A, t), $ <eq-fp>

where $hat(M)$ is the trained super-learner classifier.  At $sigma^*$,
the spatial lags are derived from the model's own equilibrium predictions,
not from historical observations.
The deterministic discrepancy disappears by construction.

This self-consistency condition is also the Nash equilibrium condition
for a natural class of games.  The primitive object is the best-response
correspondence, not the game itself: a game specifies a best-response
correspondence _derivatively_, via utility maximization; $hat(M)$ specifies
one _directly_, from the data.  Nash equilibrium is a fixed point of
the best-response correspondence and does not require identifying the
payoff functions that generate it.  Games can be partitioned into
equivalence classes under the relation "generates the same best-response
correspondence"; $sigma^*$ is the Nash equilibrium of every game in the
class whose correspondence $hat(M)$ estimates.#footnote[
  The set of rationalizing payoff functions for any interior $sigma^*$
  is non-empty---choose utilities that make each player indifferent among
  actions assigned positive probability.  What is identified is the
  correspondence; the payoffs that generate it are not.  A natural
  parametric fiber of these classes is the linear-utility family
  $u_i (a, sigma_(- i)) = alpha dot.c f_i (a) + beta dot.c W_i (a, sigma_(- i))$,
  where $f_i$ captures state-specific intervention incentives and $W_i$
  the spatial interaction term.  The best-response from this family is a
  softmax---the functional form of the multinomial logit component of
  the super-learner---so the parametric case is explicitly represented
  in the ensemble while the broader non-parametric mixture accommodates
  departures from it.  The iterative procedure that solves @eq-fp is
  structurally identical to the nested pseudo-likelihood (NPL) algorithm
  of #cite(<crismancox2021>, form: "prose"), who show that this class of estimators substantially
  outperforms direct maximum likelihood estimation of strategic games under
  equilibrium multiplicity.
]
If the training data reflect equilibrium play, $hat(M)$ is a non-parametric
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

== Predictive Performance

Table 2 reports out-of-fold performance for the super-learner and two
comparison models on the same feature set: the random forest component alone
and the ridge logistic regression (the best single logistic specification in
preliminary runs).  All statistics average across the 25 complete datasets,
with standard deviations across imputation draws in parentheses.

#figure(
  booktab(
    columns: (2fr, 1fr, 1fr, 1fr),
    headers: ("Model", "PRL", "AUC (macro)", "Log-loss"),
    rows: (
      ([Super-learner], [44.1% (0.9%)], [0.964 (0.003)], [0.038 (0.001)]),
      ([Random forest], [18.8% (5.3%)], [0.949 (0.005)], [0.056 (0.004)]),
      ([Ridge logit],   [33.6% (1.1%)], [0.954 (0.003)], [0.046 (0.001)]),
    ),
  ),
  caption: [Stage 1 classifier performance (out-of-fold, 25 imputation draws).
    Standard deviations across draws in parentheses.],
  kind: table,
) <tbl-stage1-perf>

The ensemble achieves a PRL of 44.1% and a macro-average AUC of 0.964.
For context, #cite(<carroll2019>, form: "prose") report a super-learner PRL of
0.20 predicting three-class militarized dispute outcomes from the same
CINC capability data --- itself described as a significant advance over the
capability ratio, which achieves PRL of 0.01.  A value near 0.44 indicates
that the classifier is recovering nearly half the information in intervention
outcomes that the class-frequency null discards, roughly double the best
result from the dispute-outcome literature on a structurally identical
three-class problem.

The result also speaks directly to the rational expectations assumption
embedded in the theoretical model.  The classifier is trained entirely on
publicly observable state characteristics --- material capabilities, alliance
portfolios, regime type, colonial history, foreign policy alignment.  If an
estimator operating on this strict subset of information recovers 44% of
the predictive content in realized intervention outcomes, then intervention
is, to a substantial degree, foreseeable from information available to any
careful observer.  Under rational expectations, agents exploit available
information at least as well as the analyst's model @muth1961; rebel
leaderships can additionally draw on private intelligence, direct
communications with potential patron states, diaspora networks, and
on-the-ground assessments of great power intentions that no dataset records.
The 44% figure is accordingly a lower bound on how knowable the
intervention environment is from the rebel's perspective, not a ceiling.

The performance gap between the ensemble and the ridge logistic regression ---
44.1% vs 33.6% PRL on an identical feature set --- reveals nonlinear and
interactive structure that penalised parametric models cannot recover.  The NNLS
stacking procedure quantifies this directly: it assigns near-zero weight to all
four logistic specifications (ridge, lasso, elastic-net, unpenalised
multinomial), with ensemble mass concentrating on the random forest and the deep
multi-layer perceptron (@appendix-sl, Table E1).  Because the logistic family is
explicitly included in the candidate library and explicitly down-weighted by
the data, the ensemble's superiority is not an artefact of architecture choice.

Variable importance scores --- Gini importance from the random forest component,
projected back through the PCA loadings and averaged across all 25 imputation
draws --- place the four superpower cross-spatial terms (US--USSR and USSR--US
interaction lags) in ranks 1, 9, 10, and 11 out of 105 features; the ten
spatial features collectively account for 12.4% of total projected importance
(@appendix-sl, Table E3).  The remaining top predictors --- dyadic rivalry
(rank 4), wartime alliance (rank 2), and colonial history (ranks 5 and 8) ---
are all variables with clear theoretical motivation for intervention decisions.
The spatial environment enters the prediction in first-order ways that models
ignoring strategic interdependence would miss.

A separate question is _refinement_: whether the classifier actually
discriminates at the dyad level rather than clustering all predictions near the
base rate.  Models of rare political events are routinely faulted for failing
to discriminate --- the predicted probabilities from standard logit models
rarely exceed the base rate for any observation @beck2000.
#cite(<wardgleditsch2002>, form: "prose") made the ability to generate
predictions above 0.5 the headline result of their autologistic conflict model,
noting it as qualitatively different from previous approaches.  The Stage 1
classifier achieves this for intervention: across the 19,733 onset dyad-years
in the Regan period, 3.5% of directed dyads are assigned predicted government-
or opposition-biased intervention probabilities exceeding 0.05, well above the
base rate of approximately 1.1%.  At the 99th percentile, predicted
probabilities reach 11.1% for government-biased and 9.2% for opposition-biased
intervention.  These are genuine commitments, and they are what give the
aggregate measures $E_A^G$ and $E_A^O$ their leverage in Stage 2.  A sum of 190
predictions each clustered near zero would convey little information; a sum
driven by a handful of high-conviction dyads carries the signal of the
equilibrium.  @fig-refinement illustrates this for the Angola 1975 onset.

#figure(
  image("../figures/fig-shadow-refinement.pdf", width: 95%),
  caption: [Angola 1975: top 15 predicted interveners by predicted
    government- and opposition-biased intervention probability.
    Probabilities averaged across 25 imputation draws.],
) <fig-refinement>

Because Stage 1 probabilities are subsequently summed across approximately 190
potential interveners, systematic miscalibration compounds in proportion to the
number of dyads.  NNLS stacking improves calibration relative to individual
learners; reliability diagrams confirming this for each outcome class appear in
@appendix-sl.

== Properties of the Shadow Measure <sec-shadow-properties>

Summing the dyad-level predictions across all potential interveners for each
host country-year gives the aggregate shadow measures $E_A^G$ and $E_A^O$ ---
the expected number of government- and opposition-biased interventions that
country $A$ would face in year $t$ if a civil war began (the formal definition
and aggregation details follow in the next section).  Before turning to the
onset analysis, I examine the descriptive properties of these measures to
establish that they behave as the theory predicts.

The dyad-level predictions reveal a structural asymmetry between the two
sides of intervention.  The highest predicted probabilities of
government-biased intervention involve superpowers projecting influence into
client states --- the United States supporting Laos in 1963
($hat(p)_"gov" = 0.87$), Cambodia in 1971, the Philippines in 1972.  For
opposition-biased intervention, the picture shifts: the maximum is Libya
opposing the Chadian government in 1989 ($hat(p)_"opp" = 0.63$), and 14 of
the 20 highest-ranked opposition predictions involve non-P5 interveners ---
Somalia targeting Ethiopia, Israel targeting Syria, Pakistan targeting
Afghanistan among them.  A measurement framework restricted to P5 states
would miss the majority of the opposition-biased shadow.

@tbl-shadow-dyads makes this concrete for three canonical onsets, reporting
the five states to which the classifier assigns the highest government- and
opposition-biased intervention probabilities alongside an indicator of whether
the state actually intervened in that direction.

#figure(
  booktab(
    columns: (auto, 1fr, auto, auto, 1fr, auto),
    headers: ([], [Gov-biased], [$hat(p)$], [], [Opp-biased], [$hat(p)$]),
    rows: (
      table.cell(colspan: 6, align: center,
        text(weight: "bold", [Angola (1976) --- 5 actual interventions])),
      [1], [USSR#super[$ast$]],         [.273 #sym.checkmark], [1], [Zaire],               [.515],
      [2], [Cuba],                       [.182 #sym.checkmark], [2], [South Africa],        [.216 #sym.checkmark],
      [3], [United States#super[$ast$]], [.117],                [3], [United States#super[$ast$]], [.053 #sym.checkmark],
      [4], [Portugal],                   [.061],                [4], [Portugal],             [.039],
      [5], [Zambia],                     [.046],                [5], [Congo],                [.036],
      table.cell(colspan: 6, align: center,
        text(weight: "bold", [Ethiopia (1975) --- 3 actual interventions])),
      [1], [United States#super[$ast$]], [.141],                [1], [Somalia],              [.568 #sym.checkmark],
      [2], [South Yemen],                [.122],                [2], [United States#super[$ast$]], [.098],
      [3], [USSR#super[$ast$]],          [.097 #sym.checkmark], [3], [North Yemen],          [.061],
      [4], [Kenya],                      [.029],                [4], [Sudan],                [.030],
      [5], [Sudan],                      [.026],                [5], [South Yemen],          [.009],
      table.cell(colspan: 6, align: center,
        text(weight: "bold", [Afghanistan (1978) --- 4 actual interventions])),
      [1], [USSR#super[$ast$]],          [.324 #sym.checkmark], [1], [Pakistan],             [.335 #sym.checkmark],
      [2], [United States#super[$ast$]], [.119],                [2], [United States#super[$ast$]], [.103 #sym.checkmark],
      [3], [United Kingdom#super[$ast$]],[.101],                [3], [United Kingdom#super[$ast$]], [.069],
      [4], [Iran],                       [.092],                [4], [China#super[$ast$]],    [.043],
      [5], [China#super[$ast$]],         [.072],                [5], [Iran],                  [.038 #sym.checkmark],
    ),
  ),
  caption: [Top five predicted interveners for three canonical onsets.
    Out-of-fold probabilities, averaged across 25 imputation draws.
    #super[$ast$]P5 member. #sym.checkmark indicates the state actually
    intervened in that direction (Regan coding).  Ten of 12 actual
    interventions appear in the top five on the correct side.],
) <tbl-shadow-dyads>

Across the three cases, 10 of 12 actual interventions appear in the top five
on the correct side --- and in each case, restricting the measure to P5 states
would discard the single most important opposition-biased intervener.#footnote[
  The United States appears in the top five government-biased predictions for
  all three cases, yet actually intervened on the opposition side in Angola and
  Afghanistan.  The model assigns substantial probability mass to _both_ sides
  but defaults toward government-biased intervention because that is the modal
  US posture across the full sample.  This directional ambiguity for superpowers
  is one reason the country-year aggregation is preferable to raw dyad-level
  predictions: summing across all potential interveners preserves the correct
  signal that these countries faced unusually high total intervention pressure,
  even where the classifier hedges on which side a specific superpower would take.
]
In Angola, Zaire heads the opposition-biased predictions
($hat(p)_"opp" = 0.52$), reflecting Mobutu's support for the FNLA, with
South Africa second ($0.22$); Cuba and the Soviet Union are correctly
identified on the government side.  For Ethiopia, Somalia dominates the
opposition predictions ($hat(p)_"opp" = 0.57$), consistent with the Ogaden
conflict.  For Afghanistan, Pakistan heads the opposition predictions
($hat(p)_"opp" = 0.34$) and the Soviet Union heads the government predictions
($hat(p)_"gov" = 0.32$).

Figure 3 plots $E^G$ and $E^O$ for countries whose intervention
environments are well-documented.  The cleanest case is South Vietnam, where
both measures ramp from approximately 0.6 in 1960 to $E^G = 4.3$ and
$E^O = 4.3$ at the peak of the war in 1972 --- an order of magnitude above
typical countries, reflecting the most heavily internationalized conflict
in the sample.

Angola's onset in 1975 produces the highest $E^G$ in the country's time series
($E^G = 1.68$), reflecting Soviet and Cuban support for the MPLA government.
The following year, $E^O$ spikes to 1.13 as Zaire's and South Africa's
opposition-biased involvement intensifies.  Both measures collapse after the
Cold War: by 1992, $E^G$ has fallen to 0.61.

Afghanistan illustrates a regime-switching dynamic.  Before 1976, both measures
hover near 0.6 --- a quiet intervention environment.  The 1978 Saur
Revolution raises $E^G$ to 0.94, but by 1979 --- the year of the Soviet
invasion --- $E^O$ overtakes $E^G$ ($0.94$ vs $0.66$), correctly capturing
the surge in mujahideen support from Pakistan, the United States, and others.
$E^O$ remains dominant through 2001, peaking at 1.52 in the year of the US
invasion.

Ethiopia's pattern is more complex.  $E^O$ is already elevated in the 1960s and
early 1970s, reflecting external support for the Eritrean insurgency.  The
Derg's Soviet realignment after 1974 drives $E^G$ from 0.68 (1975) to 1.56
(1978) as Cuban and Soviet government-biased support is anticipated --- but
$E^O$ does not fall correspondingly, because Eritrean and Somali opposition
pressure persists.  The result is a period of high _bilateral_ intervention
expectations rather than a clean directional flip.

Egypt provides a critical out-of-sample check: it experiences no civil war
onset in our data, so the shadow measures are pure counterfactual predictions.
The model tracks the geopolitical environment nonetheless.  The 1967 Six Day
War period produces a massive spike ($E^G = 5.0$); after the 1972 Soviet
expulsion, $E^G$ drops from 1.11 to 0.71; the 1991 Gulf War produces another
spike ($E^G = 2.38$).  That the classifier picks up these shifts for a country
that never experienced civil war onset is evidence of genuine temporal tracking,
not mere onset-fitting.

More broadly, the shadow measures are not merely recapitulating the observed
intervention sample.  Countries that never experienced civil war onset ---
including the United States, China, the United Kingdom, France, and Germany ---
generate the highest shadow values in the panel, with $E^G$ exceeding 20 for
the largest states.  This follows directly from the counterfactual
interpretation: these countries sit atop dense alliance networks, attract
intense superpower attention, and have features that the classifier correctly
associates with high intervention probability _conditional on onset_.  Non-onset
years in never-onset countries average $E^G = 3.2$, compared to $E^G = 1.1$ in
non-onset years of countries that _did_ experience onset and $E^G = 0.8$ in
onset years themselves.  Even after normalizing by the number of potential
interveners in the system, the per-dyad intervention probability runs roughly
three times higher for never-onset countries than for onset countries.  The
measure captures the intervention _environment_, not onset risk --- that
distinction is precisely the job of the Stage 2 models.

#figure(
  image("../figures/fig-shadow-ts.pdf", width: 95%),
  caption: [Shadow measure time series ($E^G$ and $E^O$) for five countries
    with well-documented intervention environments.  Probabilities averaged
    across 25 imputation draws.],
) <fig-shadow-ts>

The second exercise connects $E^G$ to the Cunningham hierarchy proxy
@cunningham2016, which is the most direct alternative measure of
anticipated government-biased intervention.  Figure 4 plots $E^G$
against the hierarchy variable for country-years where both are available.
The expected pattern is a moderate positive correlation --- the classifier
should recover the patron-client signal embedded in the Lake hierarchy ---
with systematic divergence for post-1991 years and non-superpower
conflicts where the static hierarchy proxy is least informative.  If
$E^G$ adds no information beyond hierarchy, the case for the classifier
collapses; if it diverges in theoretically predictable ways, that is
evidence the richer feature set is doing work.

#figure(
  image("../figures/fig-shadow-hierarchy.pdf", width: 85%),
  caption: [$E^G$ (asinh, trimmed) versus US security hierarchy score
    from the Lake hierarchy data used in @cunningham2016.  Each point is
    a country-year (1950--2005); binned means show the conditional trend.
    The correlation is positive ($r = 0.16$) and stronger post-Cold War
    ($r = 0.23$), confirming that the classifier recovers the
    patron-client signal while adding substantial variation from
    non-superpower interveners.],
) <fig-hierarchy>

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
#cite(<burbidge1988>, form: "prose"), I apply the inverse hyperbolic sine (asinh)
transformation before entering them into the onset models.

Because the Stage 1 classifier is trained and evaluated across 25 complete
imputed datasets, I average the predicted probabilities across datasets
before constructing the aggregate variables, propagating imputation
uncertainty without inflating the size of the second-stage estimation
sample.
