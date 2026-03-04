// ============================================================
// The Decision to Fight
// ============================================================
#import "../style.typ": booktab

= The Decision to Fight <sec-decision>

Measure in hand, I now study whether intervention expectations affect the
decision to initiate civil war.  The _Entrants_ model augments a standard
onset logit with the two shadow variables constructed in the previous
section:

$ U_O (F; hat(sigma)) = bold(X) bold(beta) + gamma^O sum_i hat(sigma)_i (O) + gamma^G sum_i hat(sigma)_i (G) + epsilon, $

where $bold(X)$ contains the baseline country-year predictors and the two
summations are the expected total number of opposition-biased and
government-biased interveners, respectively.#footnote[In estimation, the
summations enter through the inverse hyperbolic sine transform,
$"asinh"(sum_i hat(sigma)_i)$, for variance stabilization.  This
introduces a concavity---diminishing marginal returns to additional
expected interveners---that is not implied by the theory.  Because
$"asinh"$ is strictly increasing, the sign of the estimated coefficients
is invariant to this choice, and the out-of-sample model comparisons
test whether the shadow variables improve predictions regardless of
functional form.  The exact shape of the utility function over
aggregated intervention expectations is not identified from these data.]
This is the simplest
specification of @eq-EU, in which all potential interveners carry the same
utility weight ($u_O^i (O) = gamma^O$ and $u_O^i (G) = gamma^G$ for
all $i$).  I relax this assumption in the heterogeneous-utilities
extensions below.

== Empirical Strategy

Testing these models requires a comparison point.  Following
@clarke2007, the relevant question is not whether intervention
expectations matter in isolation but whether a model that includes them
outperforms one that does not.

The _Baseline_ model serves as the rival.  It is a logistic regression
for civil war onset using the covariate set from
#cite(<fearon2003>, form: "prose") Table 1, Model 1---the structural
specification the literature has converged on as a reference: prior war,
log per capita income, log population, mountainous terrain, oil exporter
status, new state status, political instability, ethnic and religious
fractionalization, and democracy---estimated on our sample (COW Intra-State Wars v5.1, 1946--2014).
This is not a replication of F&L; it is their specification used as a
comparison point on our data with updated sources.  Time-varying
covariates (income, population, democracy, political instability) are
lagged one year to avoid post-onset contamination; income and population
come from V-Dem v15 @vdem2025 and democracy from the Polity project.
Civil war history is drawn from COW v5.1.  Slowly changing or fixed
characteristics---terrain, ethnic and religious fractionalization,
colonial history---are taken from the F&L replication dataset.  To account for the mechanical growth of the summation-based
shadow variables as the state system expands, I add a year trend to all
specifications.

#cite(<cederman2010>, form: "prose") challenge the aggregate-fractionalization
baseline on the grounds that ethnopolitical exclusion from state power is
the theoretically appropriate predictor.  The Coethnics extended
specification (@sec-decision) partially addresses this critique by
interacting intervention expectations with ethnic matching between the
intervener and the host country's population.

Model comparison draws on both in-sample fit and out-of-sample
predictive performance.  Within sample, I report the proportional
reduction in log-loss relative to random guessing and the area under the
ROC curve.  Out-of-sample performance uses leave-one-war-out
cross-validation: all country-years belonging to a civil war are held
out together, and predictions are formed from the remaining data.  This
scheme respects temporal dependence and avoids the information leakage
that would arise from splitting individual country-years at random.

Because the Stage 1 probabilities are estimated quantities, I retain all
25 multiply-imputed shadow measures (5 country-year imputations $times$ 5
directed-dyad imputations) for the uncertainty analysis described below.
Point estimates for model-fit metrics are computed on the averaged shadow;
coefficient standard errors account for the full spread of measurement
draws.

== Intervention Expectations and Onset

@tbl-coefs presents the paper's central result.  Expected
government-biased intervention enters with a negative coefficient
($gamma^G = -1.66$): the prospect of outside support for the incumbent
deters rebellion.  Expected opposition-biased intervention enters
positively ($gamma^O = +0.90$): the prospect of outside support for
rebels encourages fighting.  The two sides of the shadow pull in
opposite directions, exactly as the theory requires.  As discussed
below, the corrected standard errors for these coefficients are
substantially wider than conventional MLE output would suggest; the
evidence for the directional pattern rests not on any single $p$-value
but on the constellation of signs, out-of-sample predictive gains, and
robustness to fixed effects and alternative specifications.

Because the shadow variables are generated regressors---predicted from
the Stage 1 classifier rather than directly observed---the standard
errors from ordinary logistic regression understate uncertainty.
#cite(<knoxlucascho2022>, form: "prose") show that this problem is
endemic to the learned-proxy workflow: standard practice accounts only
for primary-analysis sampling variability, ignoring the fact that the
proxy itself could have been learned differently from different training
data.  Their recommended correction runs the primary analysis separately
for each of $T$ measurement-model draws, bootstraps each $P$ times, and
pools all $T times P$ coefficient draws to construct standard errors
that reflect both sources of uncertainty.

I implement this procedure using the 25 multiply-imputed shadow
measures as the $T$ draws (each trained on a different imputed dataset)
and 200 pairs-cluster bootstrap replications as the $P$ draws, yielding
5,000 total coefficient vectors.  The corrected standard errors in
@tbl-coefs are the standard deviations of this pooled distribution.
The inflation relative to naive MLE standard errors is substantial:
roughly $2.6 times$ for $hat(E)^G$ and $2.3 times$ for $hat(E)^O$.  A
variance decomposition reveals why: approximately 70--75% of the total
coefficient variance comes from the measurement stage (variation
_between_ shadow draws), with only 25--30% from primary-analysis
sampling (variation _within_ draws).  The measurement model, not the
regression, is the binding source of uncertainty---precisely the
situation @knoxlucascho2022 warn about.  The consequence is that neither
shadow coefficient achieves conventional significance: the 95%
confidence interval for $gamma^G$ runs from $-4.11$ to $+0.01$ and for
$gamma^O$ from $-0.92$ to $+2.76$.  What the $T times P$ procedure
does _not_ change is the evidence from model comparison, to which I
turn below.

The country fixed effects specification (Column 3) absorbs all
cross-sectional confounders---terrain, ethnic composition, colonial
history, and any other time-invariant unobservable---and identifies the
shadow effect from within-country temporal variation alone.  Both
coefficients survive and strengthen: $gamma^G = -2.39$,
$gamma^O = +1.69$.  Only 71 of 171 countries exhibit variation in onset
and therefore contribute to identification; that the estimates are
larger in absolute value than the pooled logit suggests that
cross-sectional heterogeneity, if anything, attenuates the relationship.
(The conditional-logit standard errors reported in Column 3 are from
maximum likelihood and do not incorporate the $T times P$ correction;
they should be interpreted as lower bounds on uncertainty.)

How large are these effects at face value?  Taking the $T times P$ point
estimates, a one-standard-deviation increase in $hat(E)^G$ ($approx 0.80$
asinh units, corresponding to roughly two additional expected
government-biased interveners at the median country) reduces onset odds
by approximately 74%; the corresponding shift in $hat(E)^O$ ($approx
0.71$ units) raises them by roughly 90%.  Because onset is rare
($approx 2%$ of country-years), an odds reduction of this magnitude
translates to a modest absolute shift---from roughly 2% to 0.5%
predicted probability.  The shadow variables are not narrow proxies for
a single additional intervener; they aggregate a 54-component classifier
spanning capability ratios, alliance portfolios, geographic proximity,
ethnic ties, and spatial lags, so a one-standard-deviation shift
represents a wholesale repositioning of a country's strategic
environment.  Wide confidence intervals surround these magnitudes, but
the directional pattern---deterrence from government-biased
expectations, emboldening from opposition-biased expectations---is
consistent across all 25 measurement draws, all nine extended
specifications, and the fixed-effects estimator.

#figure(
  booktab(
    columns: (2.5fr, 1fr, 1fr, 1fr),
    headers: ("", "Baseline", "Entrants", "FE Entrants"),
    rows: (
      ([Polity2#sub[lag]],         [$-$0.026 \ (0.014)], [$-$0.030#super[\*] \ (0.014)], [$-$0.038#super[\*] \ (0.019)]),
      ([GDP/cap (log)#sub[lag]],   [$-$0.670#super[\*\*\*] \ (0.112)], [$-$0.521#super[\*\*\*] \ (0.123)], [$-$0.242 \ (0.268)]),
      ([Pop (log)#sub[lag]],       [+0.110 \ (0.061)], [+0.058 \ (0.068)], [+0.475 \ (0.709)]),
      ([Mountainous],              [+0.223#super[\*\*\*] \ (0.063)], [+0.071 \ (0.070)], []),
      ([Noncontiguous],            [+0.532#super[\*] \ (0.225)], [+0.622#super[\*\*] \ (0.223)], []),
      ([Oil],                      [+0.450#super[\*] \ (0.203)], [+0.377 \ (0.208)], []),
      ([New state],                [+1.003#super[\*\*] \ (0.342)], [+1.133#super[\*\*] \ (0.345)], [+1.254#super[\*\*] \ (0.386)]),
      ([Instability#sub[lag]],     [+0.567#super[\*\*] \ (0.180)], [+0.520#super[\*\*] \ (0.179)], [+0.619#super[\*\*] \ (0.196)]),
      ([Prior war],                [+0.529#super[\*\*] \ (0.195)], [+0.432#super[\*] \ (0.198)], [$-$0.349 \ (0.203)]),
      ([Ethnic frac.],             [+0.409 \ (0.293)], [+0.476 \ (0.297)], []),
      ([Religious frac.],          [+0.673 \ (0.392)], [+1.040#super[\*] \ (0.407)], []),
      ([Year],                     [+0.011#super[\*] \ (0.005)], [+0.014#super[\*\*] \ (0.005)], [+0.002 \ (0.019)]),
      table.hline(stroke: 0.5pt),
      ([$hat(E)^G$ (asinh)],       [], [$-$1.66 \ (0.98)], [$-$2.39#super[\*\*\*] \ (0.53)]),
      ([$hat(E)^O$ (asinh)],       [], [+0.90 \ (0.93)], [+1.69#super[\*\*] \ (0.54)]),
      table.hline(stroke: 0.5pt),
      ([Country FEs],              [No], [No], [Yes]),
      ([$N$],                      [8,792], [8,792], [3,750]),
      ([Onsets],                   [184], [184], [184]),
      ([PRL],                      [10.1%], [12.2%], []),
      ([AUC],                      [0.771], [0.793], []),
    ),
  ),
  caption: [Logistic regression estimates for civil war onset.  Columns 1--2
    are pooled logit; Column 3 is conditional (fixed-effects) logit.
    Point estimates and standard errors for the shadow variables
    ($hat(E)^G$, $hat(E)^O$) in Column 2 are from a $T times P$ bootstrap
    following #cite(<knoxlucascho2022>, form: "prose"): $T = 25$
    measurement-model draws $times$ $P = 200$ pairs-cluster bootstrap
    replications, pooling all 5,000 coefficient vectors.  This accounts
    for uncertainty in both the Stage 1 measurement model and the Stage 2
    primary analysis.  All other standard errors are from maximum
    likelihood.  Column 3 standard errors do not incorporate the $T times P$
    correction.  Time-invariant covariates are absorbed by country fixed
    effects in Column 3.  #super[\*] $p < 0.05$, #super[\*\*]
    $p < 0.01$, #super[\*\*\*] $p < 0.001$.],
  kind: table,
) <tbl-coefs>

The Entrants specification raises in-sample PRL from 10.1% to 12.2% and
AUC from 0.771 to 0.793.  Out-of-sample, it achieves positive
proportional reduction in log-loss under leave-one-onset-group-out
cross-validation (OOS PRL = $+0.5%$) and raises AUC from 0.650 to
0.679.  For a rare-event onset model with a 2% base rate---where the
log-loss metric penalizes any confident false positive severely---positive
OOS PRL is a nontrivial result: the Baseline specification, which reflects
decades of accumulated knowledge about structural onset risk, fails to
clear this bar.

== Heterogeneous Utilities

The Entrants model constrains all interveners to carry the same utility
weight regardless of type.  The theoretical framework permits the utility
parameters to vary with intervener characteristics: setting
$u_O^i (O) = gamma_0^O + gamma_P^O z_i$ and
$u_O^i (G) = gamma_0^G + gamma_P^G z_i$ for some intervener attribute
$z_i$ and aggregating produces the Entrants variables plus a pair of
type-weighted interaction terms.  I apply this to major-power status
(_Powers_), geographic contiguity (_Neighbors_), shared dominant ethnic
group (_Coethnics_), colonial relationship (_Rulers_), bilateral rivalry
(_Rivals_), and military balance (_DOE_).  Each type-disaggregated
specification nests the Entrants variables.

#figure(
  booktab(
    columns: (2fr, 1fr, 1fr, 1fr, 1fr),
    headers: ("Model", "IS PRL", "IS AUC", "OOS PRL", "OOS AUC"),
    rows: (
      ([Baseline],       [10.1%], [0.771], [$-$1.0%], [0.650]),
      ([Entrants],       [12.2%], [0.793], [+0.5%],   [0.679]),
      ([Powers],         [13.9%], [0.804], [+1.2%],   [0.691]),
      ([Neighbors],      [13.5%], [0.808], [+1.1%],   [0.696]),
      ([Coethnics],      [12.6%], [0.801], [+0.3%],   [0.681]),
      ([Rulers],         [12.2%], [0.793], [$-$1.4%], [0.676]),
      ([Rivals (bin)],   [13.3%], [0.804], [+0.4%],   [0.688]),
      ([Rivals (cts)],   [13.6%], [0.808], [+1.1%],   [0.692]),
      ([DOE],            [12.2%], [0.793], [+0.2%],   [0.678]),
      ([Full],           [17.7%], [0.840], [$-$2.1%], [0.725]),
    ),
  ),
  caption: [In-sample and out-of-sample fit across model specifications.
    Each extended specification nests the aggregate Entrants variables.
    IS = in-sample; OOS = leave-one-onset-group-out cross-validation.
    PRL = proportional reduction in log-loss relative to the class-frequency null.
    AUC = area under the ROC curve.],
  kind: table,
) <tbl-topfit>

Three type-disaggregated specifications---Powers, Neighbors, and
Rivals~(cts)---outperform even the Entrants model out-of-sample (OOS PRL
$approx +1.1$--$1.2%$, OOS AUC $approx 0.69$), indicating that major
powers, contiguous states, and hostile dyads carry disproportionate weight
in shaping onset expectations.  The Full model achieves the highest
discriminative ability (OOS AUC = 0.725) but overfits in terms of log-loss,
suggesting that the 26-parameter specification extracts discriminative
signal at the cost of calibration.

== The Population Puzzle

Across multiple specifications, the inclusion of intervention expectations
substantially attenuates the coefficient on log population---a predictor
that #cite(<raleigh2009>, form: "prose") characterize as ``the most robust empirical finding in
country-level studies of civil war.''

The mechanism appears to be a nonlinear relationship between population
and intervention expectations: very large states (India, China) attract
elevated intervention probabilities that partially explain the onset
events previously attributed to their size.  The intervention expectations
do not simply soak up variance; the coefficients on log per capita income,
for instance, increase in magnitude when the expectations variables are
included.

@tbl-popcoef tracks the coefficient on log population across
specifications.  In the Baseline model, the coefficient is $0.110$
($p = 0.073$)---marginally significant.  Adding the aggregate
intervention expectations (Entrants) reduces it to $0.058$ ($p = 0.39$),
a 47% attenuation.#footnote[Part of this attenuation reflects the year
trend included in all specifications.  The shadow variables grow
mechanically as the state system expands, so the year trend and log
population are correlated.  In a Baseline model without the year trend,
the population coefficient is $0.117$ ($p = 0.056$); adding the
Entrants variables without a year trend reduces it to $0.086$
($p = 0.20$)---a 27% attenuation.  The year trend thus accounts for
roughly a third of the reported 47% figure, but the shadow variables
attenuate population substantially even without it.]  Because every extended specification nests the
aggregate entrants, the additional attenuation beyond Entrants reveals
which intervener types drive the population channel.  The sharpest
reductions come from rivalry-weighted specifications: Rivals~(cts)
reverses the sign entirely ($-105%$), and Rivals~(bin) nearly
eliminates it ($-91%$).  Neighbors ($-67%$) and Rulers ($-55%$)
also show substantial attenuation, consistent with the claim that it is
the strategic intervention channel---not country size per se---that
drives the population--onset association.  Powers is the exception:
the major-power disaggregation attenuates population by only 15%,
suggesting that the population channel operates primarily through
non-superpower interveners.

#figure(
  booktab(
    columns: (2fr, 1fr, 1fr, 1fr),
    headers: ("Model", [$hat(beta)_"pop"$], "SE", "Change"),
    rows: (
      ([Baseline],       [+0.110], [(0.061)], [---]),
      ([Entrants],       [+0.058], [(0.068)], [$-$47%]),
      ([Powers],         [+0.094], [(0.069)], [$-$15%]),
      ([Neighbors],      [+0.037], [(0.069)], [$-$67%]),
      ([Coethnics],      [+0.063], [(0.069)], [$-$43%]),
      ([Rulers],         [+0.049], [(0.069)], [$-$55%]),
      ([Rivals (bin)],   [+0.010], [(0.071)], [$-$91%]),
      ([Rivals (cts)],   [$-$0.005], [(0.071)], [$-$105%]),
      ([Full],           [+0.018], [(0.078)], [$-$83%]),
    ),
  ),
  caption: [Coefficient on log population across specifications.  "Change"
    is the percentage attenuation relative to the Baseline coefficient
    (0.110).  All extended specifications nest the aggregate Entrants
    variables; additional attenuation beyond the Entrants row reflects the
    contribution of each intervener type.],
  kind: table,
) <tbl-popcoef>

== Relationship with the Literature

The theoretical framework is closest to #cite(<cunningham2016>, form: "prose"), who shows that
the prospect of government-biased superpower intervention suppresses onset.
The present paper generalizes his approach in three directions: it covers
all potential interveners (not only the dominant superpower), both sides
of intervention (not only government-biased), and derives intervention
probabilities from a calibrated predictive classifier rather than from
the structural position an intervener occupies in Lake's security
hierarchy.

#cite(<lango2023>, form: "prose") develops a related formal model in which the
threat of rebel-sided intervention can simultaneously deter onset---by
compelling governments to concede---and encourage it---by raising rebels'
expected return from fighting.  The result that onset is most likely when
domestic and international stakes are in balance is consistent with the
nonmonotone relationship that the utility parameters $gamma^O$ and
$gamma^G$ in @eq-EU can represent.

On the structural side, #cite(<gibilisco2022>, form: "prose") derive equilibrium intervention
expectations for the P5 powers from a sequential game and estimate them
via maximum simulated likelihood.  The structural approach recovers clean
counterfactual predictions and explicit identification of strategic
spillovers among the most consequential actors---advantages that require
restricting the player set to a tractable size.  The two designs are best
understood as complementary.  Where the structural game handles finer
multi-party strategic interactions within the P5, the present approach maps
the full population of potential interveners, capturing the non-P5
states---neighbors, coethnics, Cold War proxies---that constitute the
majority of actual deployments and are absent from a P5-only model by
construction.  The Nash fixed-point condition disciplines both designs; the
present approach enforces it without specifying the joint game among all $n$
players.  The connection is also embedded directly in the Stage 1 ensemble:
the unpenalised multinomial logit nests the structural functional form, and
earns stacking weight in proportion to how well that assumption fits the
data.  On time structure, the cross-sectional design averages over
1950--1999; annual estimates are necessary for explaining onset _timing_---
when violence erupts within countries whose structural risk changes little
from year to year.

On the common sample (6,825 country-years, 1950--2005), the Lake
security hierarchy score from @cunningham2016 adds no explanatory power
beyond the shadow measure: the hierarchy coefficient is effectively
zero ($p = 0.88$) when the Entrants variables are included
(@appendix-cunningham).  The shadow completely subsumes the
patron-client channel, consistent with @fig-hierarchy.

#cite(<thyne2006>, form: "prose") comes closest to the present paper's
mechanism: he provides direct evidence that
both governments and opposition groups incorporate expectations of
external support into their prewar decisions, and shows that cheap
interstate signals---day-to-day diplomatic events that shift perceived
intervention probabilities---substantially affect onset.  The present
approach differs in two respects.  It grounds intervention expectations
in a systematic predictive model rather than signal proxies, covering
the full population of potential interveners rather than politically
relevant dyads.  And it recovers the utility parameters that translate
expectations into onset propensity, rather than treating the signal
directly as the predictor.

The importance of modeling government- and opposition-biased
intervention separately is reinforced by a complementary literature on
civil war outcomes.  #cite(<sullivankarreth2014>, form: "prose") show that pro-rebel
intervention raises the probability of rebel victory by roughly 40
percentage points unconditionally, while pro-government intervention
improves government prospects only when rebels are militarily strong.
#cite(<dudley2024>, form: "prose") finds a parallel asymmetry in negotiations: rebel-biased
intervention increases the probability of talks by approximately 60
percentage points, while government-biased intervention reduces it.
Forward-looking rebels who anticipate these asymmetric downstream
effects should respond very differently to $E_A^G$ and $E_A^O$,
motivating the directional distinction as theoretically necessary
rather than merely a descriptive refinement.
