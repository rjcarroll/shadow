#import "../style.typ": booktab
// ============================================================
// Online Appendix
// ============================================================

// Typst: switch heading numbering to lettered appendix style
#set heading(numbering: "A.")

= Regan Intervention Coding Corrections <appendix-coding>

The raw #cite(<regan2000>, form: "prose") data contain ten per-conflict coding decisions that are
corrected before analysis, following Carroll (2021).  These are listed in
@tbl-regan-corrections.  In all cases the corrections are applied to the raw
Stata file _before_ country-code standardisation, matching the original R
pipeline's order of operations.

#figure(
  caption: [Regan coding corrections applied before analysis.],
  kind: table,
)[
#set text(size: 8.5pt)
#table(
  columns: (1.2fr, auto, 1fr, 1.5fr),
  stroke: none,
  inset: (x: 5pt, y: 3pt),
  table.hline(y: 0, stroke: 1.5pt),
  table.cell(align: center)[*Conflict*],
  table.cell(align: center)[*State*],
  table.cell(align: center)[*Correction*],
  table.cell(align: center)[*Rationale*],
  table.hline(y: 1, stroke: 0.75pt),
  [Congo-Brazzaville (1997)], [France (220)], [target: neutral → 2 (opp)], [France supplied arms to Cobra militia, siding with Angola/opposition],
  [Indonesia (1958)], [USA (002)], [target: gov → 2 (opp)], [CIA Operation Archipelago backed anti-communist rebels],
  [Cameroon (conflict 17)], [W. Germany], [ccode: 255 → 260], [Miscoded; correct COW code for West Germany],
  [Congo Crisis (1960s)], [All], [host ccode: 484 → 490], [DRC better represents this conflict than Republic of Congo],
  [Congo Crisis (1964)], [Belgium (211)], [target flipped gov↔opp], [Sides effectively reversed in 1964 phase],
  [Ethiopia (1980s)], [Somalia (520)], [target: gov → 2 (opp)], [Somalia generally supported Ogaden opposition],
  [Cambodia], [Vietnam (816)], [target: opp → 1 (gov)], [Vietnam supported incumbent government throughout],
  [Mozambique], [Zimbabwe (552)], [target: → 1 (gov)], [Zimbabwe consistently backed FRELIMO government],
  [Mozambique], [Malawi (553)], [target: → 3 (neutral)], [Malawi supported both sides; excluded as neutral],
  [Zimbabwe/South Africa], [South Africa], [target: → 3 (neutral)], [Supported both sides; excluded],
  [Sri Lanka], [UK (200)], [target: → 2 (opp)], [Miscoded; correct side assignment],
  [Sri Lanka], [India (750)], [target: → 1 (gov)], [India supported government],
  [Iran], [Iraq (645)], [target: gov → 1], [Iraq favored government; miscoded in original],
  table.hline(stroke: 1.5pt),
)
] <tbl-regan-corrections>

After corrections, two directed dyads with conflicting target assignments across
multiple records are resolved by fixing the target directly: Georgia--Russia
(opposition-biased) and Liberia--Sierra Leone (government-biased).  Final
deduplication retains the first record per directed dyad-year after sorting by
the `ddyear` identifier.

== Post-1999 Intervention Coding <appendix-post1999>

The @regan2000 data end in 1999.  To extend the Stage 1 training set through 2014,
I code military interventions in all 38 post-1999 COW onset country-years using the
same threshold: deployment of military personnel (troops, combat advisors, or
directed proxy forces) attributable to a specific state, on behalf of one side.
Arms transfers, economic aid, sanctuary, and strategic direction alone are
insufficient.  UN peacekeeping missions are excluded.  @tbl-post1999 lists
the 32 coded interventions; the remaining 17 onset country-years are coded as
having no intervention meeting the threshold.  Full source notes for each
coding decision are available in the replication archive.

#figure(
  caption: [Post-1999 military interventions in COW onset country-years.
    Gov.~=~government-biased; Opp.~=~opposition-biased.],
  kind: table,
)[
#set text(size: 8.5pt)
#table(
  columns: (1.4fr, auto, 1.4fr, auto),
  stroke: none,
  inset: (x: 5pt, y: 3pt),
  table.hline(y: 0, stroke: 1.5pt),
  [*Host*], [*Year*], [*Intervener*], [*Side*],
  table.hline(y: 1, stroke: 0.75pt),
  [Sierra Leone],  [2000], [United Kingdom],  [Gov.],
  [Ivory Coast],   [2002], [France],          [Gov.],
  [Liberia],       [2002], [Guinea],          [Opp.],
  [Pakistan],      [2004], [United States],   [Gov.],
  [Chad],          [2005], [France],          [Gov.],
  [Chad],          [2005], [Sudan],           [Opp.],
  [Pakistan],      [2005], [United States],   [Gov.],
  [Somalia],       [2006], [Ethiopia],        [Gov.],
  [Chad],          [2007], [France],          [Gov.],
  [Chad],          [2007], [Sudan],           [Opp.],
  [Pakistan],      [2007], [United States],   [Gov.],
  [Somalia],       [2009], [Ethiopia],        [Gov.],
  [Iraq],          [2010], [United States],   [Gov.],
  [Ivory Coast],   [2011], [France],          [Opp.],
  [Libya],         [2011], [United States],   [Opp.],
  [Libya],         [2011], [United Kingdom],  [Opp.],
  [Libya],         [2011], [France],          [Opp.],
  [Libya],         [2011], [Qatar],           [Opp.],
  [Libya],         [2011], [UAE],             [Opp.],
  [Mali],          [2012], [France],          [Gov.],
  [Mali],          [2012], [Chad],            [Gov.],
  [Nigeria],       [2013], [Chad],            [Gov.],
  [South Sudan],   [2013], [Uganda],          [Gov.],
  [Ukraine],       [2014], [Russia],          [Opp.],
  [Somalia],       [2014], [Ethiopia],        [Gov.],
  [Somalia],       [2014], [Uganda],          [Gov.],
  [Libya],         [2014], [Egypt],           [Gov.],
  [Libya],         [2014], [UAE],             [Gov.],
  [Yemen],         [2014], [Saudi Arabia],    [Gov.],
  [Yemen],         [2014], [UAE],             [Gov.],
  [Afghanistan],   [2014], [United States],   [Gov.],
  [Afghanistan],   [2014], [United Kingdom],  [Gov.],
  table.hline(stroke: 1.5pt),
)
] <tbl-post1999>

Onset country-years coded as having no military intervention meeting the
personnel-deployment threshold: Guinea (2000), Philippines (2000, 2003, 2005),
Burundi (2001), Rwanda (2001), Nepal (2001, 2003), Sudan (2003, 2011),
Indonesia (2003), Yemen (2004, 2007), Sri Lanka (2006), Syria (2011),
Myanmar (2011), and Central African Republic (2012).

= Multiple Imputation Procedure <appendix-imputation>

Missing values in continuous predictors are handled through a two-stage
multiple imputation strategy using miceforest (Multiple Imputation by Chained
Equations with LightGBM).  The strategy generates $5 times 5 = 25$ complete
datasets; estimates are combined across datasets following @rubin1987.

The staged structure reflects the data's own hierarchy and is a design
choice, not a computational convenience.  Country-year quantities (GDP per
capita, regime scores, CINC components) must be imputed at the country-year
level: imputing them at the directed-dyad level would treat state $A$'s GDP
as a separate quantity in each of $A$'s dyads, producing inconsistent imputed
values across rows that share the same country and year.  Dyadic quantities
(bilateral trade, peace scores, capital distances) must be imputed before
directed expansion so that the symmetry constraint $x_(A B) = x_(B A)$ holds
within each imputed dataset---a constraint that would be broken if each
directed-dyad row were imputed independently.  Imputing in sequence,
country-year first and undirected dyad second, preserves both requirements
simultaneously.

*Stage 1 — Country-year imputation.*  The analysis sample (1946--2014, $n =
8{,}897$ country-years) is imputed five times.  Variables imputed include
regime scores, GDP per capita, population, oil wealth, mountainous terrain,
ethnic fractionalization, ethnic exclusion, all six NMC capability components,
and UN ideal points.  Six right-skewed NMC variables are asinh-transformed
before imputation (with individually optimised scale parameters minimising the
Kolmogorov-Smirnov statistic against the standard normal) and back-transformed
afterwards.  Five iterations of chained equations are run.  Post-imputation
clipping enforces substantive constraints ($-10 <= $ `polity2` $ <= 10$,
probabilities $in [0,1]$, capabilities $>= 0$).

*Stage 2 — Undirected-dyad imputation.*  For each of the five country-year
datasets, the undirected dyad frame (~599,000 rows) is imputed five times before
expansion to directed dyads.  Imputation at the undirected-dyad stage ensures
that symmetric quantities (trade flows, peace scores, capital distance) satisfy
$x_(A B) = x_(B A)$ by construction within each imputed dataset.  Variables
imputed include bilateral trade flows, peace quality scores, capital distances,
and dispute outcome expectations.  Two iterations of chained equations are run.

The 25 resulting complete datasets are processed identically through the
spatial-weights and intervention-coding stages before combination.#footnote[
  The 25 branches are mutually independent at every stage upstream of the
  final combination: each (cy, ud) pair flows from imputation through
  Stage 1 training, Stage 1 prediction, and Stage 2 estimation without
  reference to any other branch.  Rubin's Rules averaging is the only step
  that requires all 25 to be complete.  This means the pipeline is
  embarrassingly parallel---all 25 classifiers could in principle be trained
  simultaneously---and that updating the sample in a future study would not
  require retraining all 25 models from scratch.  A single additional
  imputed dataset, processed through the same pipeline, contributes
  marginally to the final combination.
]

= Variable List <appendix-varlist>

@tbl-cy-vars lists all country-year variables included in the analysis.
@tbl-dyad-vars lists the dyadic variables added at the directed-dyad stage.

#figure(
  caption: [Country-year variables.],
  kind: table,
)[
#set text(size: 9pt)
#table(
  columns: (auto, 1fr, auto, auto),
  stroke: none,
  inset: (x: 5pt, y: 3pt),
  table.hline(y: 0, stroke: 1.5pt),
  table.cell(align: left)[*Variable*],
  table.cell(align: left)[*Description*],
  table.cell(align: left)[*Source*],
  table.cell(align: left)[*Coverage*],
  table.hline(y: 1, stroke: 0.75pt),
  [`onset`], [Civil war onset (COW types 4--5, $>=$ 1,000 battle deaths)], [COW Intra-State Wars v5.1], [1946--2014],
  [`polity2`], [Polity II score ($-10$ to $+10$)], [V-Dem v15 (`e_polity2`)], [1946--2014],
  [`v2x_polyarchy`], [Electoral democracy index (0--1)], [V-Dem v15], [1946--2014],
  [`instab`], [Political instability (large Polity swing or coup)], [V-Dem v15], [1946--2014],
  [`lgdp`], [Log GDP per capita (2011 PPP int'l \$)], [V-Dem v15 (`e_gdppc`)], [1946--2014],
  [`lpop`], [Log population (thousands)], [V-Dem v15 / NMC v6 backup], [1946--2014],
  [`oil`], [Oil/gas income per capita $>0$ (binary)], [V-Dem v15 (`e_total_oil_income_pc`)], [1946--2014],
  [`cinc`], [Composite Index of National Capability], [COW NMC v6], [1946--2014],
  [`milex`], [Military expenditure (thousands USD)], [COW NMC v6], [1946--2014],
  [`milper`], [Military personnel (thousands)], [COW NMC v6], [1946--2014],
  [`major_power`], [COW major power status (binary)], [COW Major Powers 2024], [1946--2014],
  [`is_P5`], [UN Security Council P5 member (binary)], [Coded from COW ccodes], [1946--2014],
  [`ideal_point`], [UNGA ideal point (liberal--conservative)], [Voeten et al. 2024 @bailey2017], [1946--2023],
  [`recent_int`], [Military interventions by this state in prior 5 years], [Regan (2000) @regan2000], [1946--1999],
  [`lmtnest`], [Log(\% mountainous terrain + 1)], [Fearon & Laitin (2003) @fearon2003], [time-invariant],
  [`ncontig`], [Non-contiguous state (islands, exclaves)], [Fearon & Laitin (2003)], [time-invariant],
  [`colbrit`], [Former British colony], [Fearon & Laitin (2003)], [time-invariant],
  [`colfra`], [Former French colony], [Fearon & Laitin (2003)], [time-invariant],
  [`ethfrac`], [Ethnic fractionalization index (0--1)], [Fearon & Laitin (2003)], [time-invariant],
  [`eth_excl_frac`], [Population share of excluded ethnic groups], [EPR 2021 @cederman2010], [1946--2021],
  [`nwstate`], [New state (independent $<=$ 2 years)], [COW States 2011], [1946--2014],
  [`prior_war`], [Ongoing civil war in previous year], [COW Intra-State Wars v5.1], [1946--2014],
  table.hline(stroke: 1.5pt),
)
] <tbl-cy-vars>

#figure(
  caption: [Directed-dyad-year variables (B = potential intervener, A = host).],
  kind: table,
)[
#set text(size: 9pt)
#table(
  columns: (auto, 1fr, auto, auto),
  stroke: none,
  inset: (x: 5pt, y: 3pt),
  table.hline(y: 0, stroke: 1.5pt),
  table.cell(align: left)[*Variable*],
  table.cell(align: left)[*Description*],
  table.cell(align: left)[*Source*],
  table.cell(align: left)[*Coverage*],
  table.hline(y: 1, stroke: 0.75pt),
  [`ud_defense`], [Defense alliance (binary)], [COW Alliances v4.1 @gibler2009], [1946--2012],
  [`ud_entente`], [Entente alliance (binary)], [COW Alliances v4.1], [1946--2012],
  [`ud_A_biImports`], [Host's imports from intervener (millions USD)], [COW Trade v4.0], [1946--2014],
  [`ud_log_capdist`], [Log capital distance (km)], [COW capdist], [static],
  [`ud_conttype`], [Contiguity type (1=land, ..., 6=none)], [COW Contiguity v3.2 @stinnett2002], [1946--2016],
  [`ud_peace`], [Peace quality score (0--1)], [Diehl et al. v2.01 @diehl2019], [1946--2015],
  [`ud_icow_nclaims`], [Number of active territorial claims], [ICOW v10.1 @huth2003], [1946--2001],
  [`igo_shared`], [Shared full IGO memberships (count)], [COW IGO v3 @pevehouse2020], [1946--2014],
  [`ideal_point_distance`], [Absolute UNGA ideal-point difference], [Derived from `ideal_point`], [1946--2023],
  [`doe_pr_win_A`], [P(A wins bilateral dispute)], [Carroll & Kenkel DOE v2.0 @carroll2019], [1946--2012],
  [`A_wasColOf_B`], [A was a colony of B (binary)], [COW coldata], [static],
  [`B_wasColOf_A`], [B was a colony of A (binary)], [COW coldata], [static],
  [`ud_sharedColonizer`], [Shared former colonizer (binary)], [COW coldata], [static],
  [`ud_sameFirstEth`], [Same dominant ethnic group (binary)], [Ellingsen (2000)], [1945--2002],
  [`log_capratio`], [Log(cinc_B / cinc_A) capability ratio], [Derived from NMC v6], [1946--2014],
  [`spat_gov`], [Polity-weighted fraction of other interveners: gov-biased], [Derived (see App. D)], [1946--1999],
  [`spat_opp`], [Polity-weighted fraction: opp-biased], [Derived (see App. D)], [1946--1999],
  [`spat_US_G`], [USA is gov-biased in this conflict (binary)], [Derived], [1946--1999],
  [`spat_USSR_G`], [USSR/Russia is gov-biased (binary)], [Derived], [1946--1999],
  [`spat_US_O`], [USA is opp-biased (binary)], [Derived], [1946--1999],
  [`spat_USSR_O`], [USSR/Russia is opp-biased (binary)], [Derived], [1946--1999],
  [`spat_US_USRG`], [B=USA and USSR gov-biased (binary)], [Derived], [1946--1999],
  [`spat_US_USRO`], [B=USA and USSR opp-biased (binary)], [Derived], [1946--1999],
  [`spat_USR_USG`], [B=USSR and USA gov-biased (binary)], [Derived], [1946--1999],
  [`spat_USR_USO`], [B=USSR and USA opp-biased (binary)], [Derived], [1946--1999],
  table.hline(stroke: 1.5pt),
)
] <tbl-dyad-vars>

= Spatial Weights Derivation <appendix-spatial>

For each year $t$ in the analysis window, I construct a row-normalised
polity-similarity weight matrix $W_t$ over the set of potential intervener
states.  Let $p_i$ denote the Polity II score of state $i$ in year $t$.  The
raw weight is:

$ w_(i j) = cases(
  1 slash |p_i - p_j| & "if" p_i eq.not p_j "and" i eq.not j,
  0                   & "otherwise"
) $

Row-normalization gives $W_(i j) = w_(i j) / (sum_k w_(i k))$, so that each row
sums to 1 (rows summing to zero remain zero).  States with missing Polity scores
are excluded from $W_t$ in that year.

The polity-similarity weighting encodes the assumption that states pay more
attention to the intervention choices of ideologically similar peers, and less
to the choices of ideological adversaries.  This departs from the standard
inverse-distance spatial weight used in most spatial-lag models, which would
give _higher_ weight to more _different_ states; the similarity-based weight is
theoretically more appropriate for the ideological-diffusion channel.

Yemen (COW code 678) is excluded from the 1990 weight matrix to avoid
anomalous results during the unification transition year.

For each onset event (host country $A$, year $t$), the spatial lag for potential
intervener $B$ is the weighted sum of other potential interveners' predicted
intervention probabilities, with $A$ removed from the weight matrix before
computation:

$ "spat_gov"_B = sum_(j eq.not A, j eq.not B) W_(B j) dot hat(sigma)_j^* (1), $

where $hat(sigma)_j^*(1)$ is the Nash equilibrium probability that state $j$
intervenes on behalf of the government, as produced by the fixed-point iteration
described in @sec-constructing.  An analogous expression gives `spat_opp`
using $hat(sigma)_j^*(2)$.

In the first training iteration (initialisation), $hat(sigma)_j^*(k)$ is
replaced by the observed indicator $bb(1)[y_j = k]$, where $y_j in {0, 1, 2}$
is the Regan-coded intervention outcome.  Subsequent iterations substitute the
model's own out-of-fold predictions until convergence, at which point the lags
are self-consistent with the predictions they condition on.

= Stage 1 Classifier Diagnostics <appendix-sl>

This appendix provides the extended diagnostic output for the Stage 1
super-learner that is summarised in the main text.  All statistics are
out-of-fold unless stated otherwise; all figures and tables average across
the 25 complete imputation datasets (5 country-year draws $times$ 5 dyad draws)
with standard deviations in parentheses or as error bars.

== Component Learner Performance

#figure(
  booktab(
    columns: (2fr, 1fr, 1fr, 1fr),
    headers: ("Learner", "PRL", "AUC (macro)", "Log-loss"),
    rows: (
      ([Random forest],   [18.8%],     [0.949 (0.005)], [0.056 (0.004)]),
      ([HGB (lr=0.10)],   [$-$58.5%],  [0.878 (0.024)], [0.109 (0.028)]),
      ([HGB (lr=0.05)],   [28.1%],     [0.953 (0.005)], [0.049 (0.001)]),
      ([Ridge],           [33.6%],     [0.954 (0.003)], [0.046 (0.001)]),
      ([Elastic net],     [34.2%],     [0.953 (0.003)], [0.045 (0.001)]),
      ([Lasso],           [35.1%],     [0.955 (0.003)], [0.045 (0.001)]),
      ([Multinomial],     [32.5%],     [0.953 (0.003)], [0.046 (0.001)]),
      ([MLP (25)],        [$-$2.1%],   [0.820 (0.077)], [0.070 (0.016)]),
      ([MLP (100, 50)],   [30.4%],     [0.927 (0.022)], [0.048 (0.004)]),
      ([*Super-learner*], [*44.1%*],   [*0.964 (0.003)*], [*0.038 (0.001)*]),
    ),
  ),
  caption: [Performance of all nine component learners and the
    super-learner ensemble (out-of-fold, averaged across 25 imputation
    draws).  Standard deviations across draws in parentheses.],
  kind: table,
) <tbl-sl-components>

== NNLS Ensemble Weights

#figure(
  booktab(
    columns: (2fr, 1fr, 1fr, 1fr, 1fr),
    headers: ("Learner", "Mean", "SD", "Min", "Max"),
    rows: (
      ([Random forest], [54.1%], [7.6%], [41.4%], [66.0%]),
      ([MLP (100, 50)], [25.0%], [10.4%], [9.5%],  [46.0%]),
      ([HGB (lr=0.05)], [13.8%], [6.4%], [1.2%],  [26.7%]),
      ([Multinomial],   [4.0%],  [4.3%], [0.0%],  [14.7%]),
      ([HGB (lr=0.10)], [2.0%],  [2.5%], [0.0%],  [7.8%]),
      ([MLP (25)],      [0.9%],  [1.6%], [0.0%],  [4.5%]),
      ([Ridge],         [0.2%],  [1.1%], [0.0%],  [5.7%]),
      ([Elastic net],   [0.0%],  [0.1%], [0.0%],  [0.6%]),
      ([Lasso],         [0.0%],  [0.0%], [0.0%],  [0.0%]),
    ),
  ),
  caption: [NNLS super-learner weights across 25 imputation draws.],
  kind: table,
) <tbl-sl-weights>

== Calibration

Because Stage~1 probabilities are summed across approximately 190 dyads to
form the country-year shadow measure, a calibration error of $delta$ per dyad
compounds to roughly $190 delta$ in the aggregate --- so even small
miscalibration matters.  @fig-sl-calibration presents reliability diagrams
for each outcome class, pooled across all 25 imputation draws.  In each
panel, observations are sorted by predicted probability and divided into
10 quantile bins; the observed frequency within each bin is plotted against
the mean prediction.  The super-learner is well calibrated across all three
classes: points track the diagonal closely, with no systematic over- or
under-prediction.

#figure(
  image("../figures/fig-sl-calibration.pdf", width: 100%),
  caption: [Reliability diagrams for each outcome class (no intervention,
    government-biased, opposition-biased).  Each panel plots observed
    frequency against mean predicted probability in 10 quantile bins,
    pooled across all 25 imputation draws; the dashed diagonal represents
    perfect calibration.  Axes are zoomed to the range where data reside.
    Shaded histograms show the distribution of predicted probabilities.],
) <fig-sl-calibration>

== Fixed-Point Convergence <appendix-fp>

=== The Consistency Requirement

The spatial lags `spat_gov` and `spat_opp` enter the Stage 1 classifier as
predictors: they summarize how other potential interveners in the same conflict
are behaving, weighted by ideological similarity.  But the classifier itself
produces predicted intervention probabilities, and it is those probabilities
--- rather than the realized 0/1/2 outcomes --- that should, in principle,
enter the spatial weights.  The reason is that observed outcomes are single
draws from mixed-strategy equilibrium probabilities; a government-biased
intervention (outcome 1) carries no information about whether the intervening
state was playing a pure or mixed strategy.

Formally, the requirement is _self-consistency_: the spatial lags fed into
$hat(M)$ should be the weighted sums of the probabilities that $hat(M)$ itself
assigns to each dyad.  Let $hat(bold(p)) = hat(M)(bold(X)(bold(s)))$ be the
vector of predicted probabilities, where $bold(X)(bold(s))$ denotes the
feature matrix with spatial lags computed from $bold(s)$.  Self-consistency
requires $bold(s) = W hat(bold(p))$, a fixed-point condition.

=== Diagnosis of Non-Convergence

The main training loop (@sec-constructing) runs five rounds of
retrain-then-update, initialising $bold(s)^{(0)}$ from the realized binary
outcomes.  To assess whether five rounds are sufficient, I compute the mean
absolute change in spatial lags between the final iteration and the
initialisation:

$ Delta = 1/n sum_i |s_i^{(5)} - s_i^{(0)}| $

Averaging across all onset rows and both lag channels, $Delta approx 0.006$
after five iterations.  The theoretical tolerance threshold for self-consistency
is $10^(-4)$; five iterations leave the spatial lags 57 times above threshold.

=== Frozen-Model Burnout

Retraining the full super-learner is computationally expensive (~3 hours per
draw).  An alternative is _frozen-model burnout_: after training is complete,
hold the fitted $hat(M)$ fixed and iterate spatial lags using its predictions
until convergence.  Each burnout pass costs only a prediction sweep
(milliseconds versus hours).

The burnout algorithm is:
#set enum(indent: 1em)
+ Initialise from the five-iteration approximation $bold(s)^{(5)}$.
+ Apply $hat(M)$ to features $bold(X)(bold(s)^{(k)})$ to obtain $hat(bold(p))^{(k)}$.
+ Update $bold(s)^{(k+1)} = W hat(bold(p))^{(k)}$.
+ If $max_i |s_i^{(k+1)} - s_i^{(k)}| < tau$, stop; otherwise go to step 2.

The tolerance is set at $tau = 5 times 10^{-4}$.  This is above the
theoretical $10^{-4}$ threshold because random-forest predictions have
intrinsic stochasticity (different prediction calls can return slightly different
probabilities for the same input due to tree-averaging over bootstrap samples).
Empirical experiments confirm that $Delta$ plateaus around $5 times 10^{-4}$ and
cannot be reduced further without retraining.

Across all 25 imputation draws, burnout converges in 4--11 iterations (mean
5.4), with final $Delta$ ranging from $3.2 times 10^{-4}$ to
$5.0 times 10^{-4}$ --- confirming that the plateau reflects irreducible
prediction noise rather than failure to converge.  @fig-fp-convergence shows the
convergence trajectories: all draws exhibit rapid geometric decay (roughly
halving each iteration) before plateauing at the tolerance threshold.

#figure(
  image("../figures/fig-fp-convergence.pdf", width: 85%),
  caption: [Burnout convergence across 25 imputation draws.  Lines show the
  per-iteration trajectory of mean $|Delta "spatial lag"|$ for the 13 draws with
  full iteration histories; dots mark endpoints for the remaining 12 draws.
  Dashed red line: convergence tolerance ($5 times 10^(-4)$).],
) <fig-fp-convergence>

=== Interpretation: Approximate Equilibrium Selection

The burnout iteration does not find an equilibrium of the underlying
intervention game; it finds a fixed point of the _fitted classifier_ $hat(M)$.
These coincide only if $hat(M)$ exactly recovers the true equilibrium mapping,
which no finite-sample estimator can guarantee.

The weaker, defensible claim is as follows.  Realized outcomes are draws from
_some_ equilibrium of the underlying game --- they satisfy Nash consistency by
assumption, since they were generated by the players.  But the equilibrium
correspondence for a game of this scale (up to 190 potential interveners, each
choosing a mixed strategy) generically admits multiple equilibria, and different
conflict cases may be near different connected components.  The realized binary
outcomes provide only coarse information about which equilibrium each case is
near: a 0/1/2 outcome is consistent with any mixed strategy that places positive
probability on that action.

The burnout iteration uses the model to extract additional information.  Given
the realized action as a starting condition, iterating the model refines the
estimated spatial lags toward values that are approximately consistent with the
smooth probability distribution implied by $hat(M)$.  In this sense, the
iteration performs _approximate equilibrium selection_: it identifies, for each
conflict case, which region of the equilibrium correspondence the realized
outcome is most consistent with, according to the model's learned mapping.

Three caveats are important.  First, the model is trained on spatial lags
derived from realized outcomes (the five-iteration warm start), not from the
burnout-converged values.  Retraining on the converged values would raise a
circularity concern: the model's weights would depend on predictions the model
itself generated.  By freezing the model before burnout, the learned coefficients
remain anchored to the training-time feature distribution, and the converged
lags represent a small perturbation ($Delta approx 0.003$ relative to the warm
start) rather than a wholesale shift to a foreign equilibrium.  Second, the
equilibrium selected for each training case by the burnout need not be the same
equilibrium that governs prediction-time cases.  The identifying assumption is
that the equilibrium selection mechanism is stable across the sample---the same
forces that push cases toward particular equilibria in training also govern
unobserved cases.  Third, the burnout-converged lags are used only to compute
the shadow measure (the country-year expectations $E_"gov"$ and $E_"opp"$ in
Stage 2) and not to re-estimate Stage 1.  The shadow measure therefore reflects
the approximately-equilibrium spatial environment, while the classifier's
weights remain from training on the realized-outcome initialization.

== Variable Importance

Because the super-learner operates on PCA-transformed features, standard
permutation importance is not directly applicable to raw inputs.  Instead,
I project the random forest's Gini importance scores back through the PCA
loadings: for each raw feature $j$, projected importance equals
$sum_k I_k dot ell_(k j)^2$, where $I_k$ is the RF importance of
principal component $k$ and $ell_(k j)$ is the loading of feature $j$ on
component $k$.  This captures each feature's contribution to predictive
performance _through_ the dimensionality reduction, averaged across all
25 imputation draws.  Table E3 reports the top 20 features; spatial lag
features are flagged with $ast$.

#figure(
  booktab(
    columns: (auto, 1fr, auto, auto),
    headers: ([Rank], [Feature], [Importance], [SD]),
    rows: (
      [1],  [$ast$ spat_US_USRO],     [0.0191], [0.0015],
      [2],  [ud_iswar_allied],         [0.0184], [0.0008],
      [3],  [instab_B],               [0.0179], [0.0009],
      [4],  [ud_peace],               [0.0179], [0.0007],
      [5],  [A_wasColOf_B],           [0.0175], [0.0005],
      [6],  [ud_sharedColonizer],     [0.0174], [0.0007],
      [7],  [ud_neutrality],          [0.0171], [0.0006],
      [8],  [B_wasColOf_A],           [0.0171], [0.0010],
      [9],  [$ast$ spat_US_USRG],     [0.0168], [0.0024],
      [10], [$ast$ spat_USR_USG],     [0.0167], [0.0015],
      [11], [$ast$ spat_USR_USO],     [0.0164], [0.0014],
      [12], [ud_sameFirstRel],        [0.0164], [0.0007],
      [13], [lmtnest_B],             [0.0158], [0.0005],
      [14], [ud_sameFirstEth],        [0.0157], [0.0007],
      [15], [nwstate_B],             [0.0157], [0.0008],
      [16], [instab_A],              [0.0152], [0.0011],
      [17], [eth_excl_frac_B],       [0.0150], [0.0006],
      [18], [relfrac_B],             [0.0148], [0.0005],
      [19], [onset_B],               [0.0144], [0.0005],
      [20], [colfra_B],              [0.0129], [0.0004],
    ),
  ),
  caption: [Projected variable importance from the random forest component of the super-learner (top 20 of 105 features). Importance scores are Gini importance on PCA components projected through PCA loadings ($sum_k I_k dot ell_(k j)^2$), normalized to sum to 1 and averaged across 25 imputation draws.  $ast$ denotes spatial lag features constructed via the Nash fixed-point. The ten spatial features collectively account for 12.4% of total importance.],
) <tbl-sl-importance>

= Comparison with the Cunningham Hierarchy <appendix-cunningham>

#cite(<cunningham2016>, form: "prose") measures anticipated government-biased
intervention using the Lake security hierarchy, a cross-sectional index of
U.S.~patron-client relationships.  @tbl-cunningham merges the hierarchy
variable with the shadow measure on the common sample (6,825 country-years,
1950--2005, restricted to non-missing hierarchy scores) and compares four
logistic onset models.

#figure(
  booktab(
    columns: (2.5fr, 1fr, 1fr, 1fr, 1fr),
    headers: ("Model", "PRL", "AUC", [$hat(beta)_"hier"$], [$p$]),
    rows: (
      ([Baseline],                [10.9%], [0.781], [---],     [---]),
      ([Base + Hierarchy],        [10.9%], [0.781], [$-$0.09], [0.82]),
      ([Entrants],                [12.5%], [0.798], [---],     [---]),
      ([Entrants + Hierarchy],    [12.5%], [0.798], [+0.05],  [0.88]),
    ),
  ),
  caption: [Onset model comparison with the Lake security hierarchy from
    @cunningham2016.  Common sample: 6,825 country-years, 1950--2005.  The
    hierarchy score has no predictive power for onset either alone
    (row 2) or conditional on the shadow variables (row 4).],
  kind: table,
) <tbl-cunningham>

The hierarchy score adds nothing to the Baseline model (PRL and AUC are
unchanged), and conditional on the shadow variables its coefficient is
effectively zero ($p = 0.88$).  The shadow measure completely subsumes
whatever onset-relevant information the hierarchy proxy contains.  This
is consistent with @fig-hierarchy in the main text: the classifier recovers
the patron-client signal embedded in the Lake hierarchy while adding
substantial variation from non-superpower interveners, bilateral
relationships, and the spatial environment.

= Flexible Second-Stage Learners <appendix-ml-stage2>

The main text estimates the onset model as a logistic regression, which
constrains the shadow variables to enter linearly and additively.  This
appendix asks whether flexible learners---a random forest (500 trees) and
a histogram-based gradient boosting classifier---improve on the logit when
given the same features.  All three learners are evaluated under
leave-one-group-out cross-validation, identical to the scheme used in the
main text.

#figure(
  booktab(
    columns: (2fr, 1fr, 1fr, 1fr, 1fr),
    headers: ("", [Baseline], [], [+ Shadow], []),
    rows: (
      ([], [PRL], [AUC], [PRL], [AUC]),
      table.hline(stroke: 0.5pt),
      ([Logit],   [$-$0.7%], [0.647], [+0.8%], [0.677]),
      ([Random forest],  [$-$53.3%], [0.534], [$-$38.7%], [0.578]),
      ([Gradient boosting], [$-$84.9%], [0.596], [$-$67.6%], [0.634]),
    ),
  ),
  caption: [Out-of-sample performance (leave-one-group-out) of three
    second-stage learners, with and without the shadow variables.  PRL =
    proportional reduction in log-loss.],
  kind: table,
) <tbl-ml-stage2>

Two findings emerge.  First, the logit dominates both flexible learners
out-of-sample: the random forest and gradient boosting classifier
massively overfit, achieving in-sample PRL above 40% but negative OOS PRL.
With 183 onsets in approximately 8,700 country-years, the onset signal is
too sparse for tree-based methods to generalise---a result consistent with
@ward2010's finding that flexible learners require careful regularisation
in rare-event settings.  Second, adding the shadow variables improves all
three learners: the random forest's OOS PRL rises by 15 percentage points
and the gradient boosting classifier's by 17 points.  The shadow carries
genuine predictive content even in a model class unconstrained by the
linear-additive assumption.

@tbl-ml-importance reports permutation importance from the random forest
fitted with all features.  The two shadow variables rank first and second,
ahead of log GDP per capita---the single strongest predictor in the
baseline set.  This ranking is consistent with the logit results: the
shadow variables carry more information about onset than any individual
structural predictor.

#figure(
  booktab(
    columns: (auto, 2fr, 1fr, 1fr),
    headers: ("Rank", "Feature", "Importance", "SD"),
    rows: (
      ([1], [$hat(E)^G$ (asinh)],     [0.0210], [0.0004]),
      ([2], [$hat(E)^O$ (asinh)],     [0.0195], [0.0003]),
      ([3], [GDP/cap (log)],           [0.0151], [0.0003]),
      ([4], [Ethnic frac.],            [0.0119], [0.0003]),
      ([5], [Pop (log)],               [0.0111], [0.0003]),
      ([6], [Polity2],                 [0.0099], [0.0004]),
      ([7], [Mountainous],             [0.0096], [0.0003]),
      ([8], [Religious frac.],         [0.0088], [0.0002]),
      ([9], [Year],                    [0.0078], [0.0001]),
      ([10], [Prior war],              [0.0057], [0.0002]),
    ),
  ),
  caption: [Permutation importance from a random forest trained on
    baseline covariates plus shadow variables (top 10 of 14 features).
    Importance is the increase in log-loss when each feature is permuted,
    averaged over 30 permutations.],
  kind: table,
) <tbl-ml-importance>
