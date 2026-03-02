// ============================================================
// Online Appendix
// ============================================================

// Typst: switch heading numbering to lettered appendix style
#set heading(numbering: "A.")

= Regan Intervention Coding Corrections <appendix-coding>

The raw @regan2000 data contain ten per-conflict coding decisions that are
corrected before analysis, following Carroll (2021).  These are listed in
@tbl-regan-corrections.  In all cases the corrections are applied to the raw
Stata file _before_ country-code standardisation, matching the original R
pipeline's order of operations.

#figure(
  caption: [Regan coding corrections applied before analysis.],
  kind: table,
)[
#table(
  columns: (auto, auto, auto, auto),
  stroke: none,
  inset: (x: 6pt, y: 4pt),
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
  [Iran], [Iraq (645)], [target: gov → 1], [Iraq favoured government; miscoded in original],
  table.hline(stroke: 1.5pt),
)
] <tbl-regan-corrections>

After corrections, two directed dyads with conflicting target assignments across
multiple records are resolved by fixing the target directly: Georgia--Russia
(opposition-biased) and Liberia--Sierra Leone (government-biased).  Final
deduplication retains the first record per directed dyad-year after sorting by
the `ddyear` identifier.

= Multiple Imputation Procedure <appendix-imputation>

Missing values in continuous predictors are handled through a two-stage
multiple imputation strategy using miceforest (Multiple Imputation by Chained
Equations with LightGBM).  The strategy generates $5 times 5 = 25$ complete
datasets, propagating both country-year and undirected-dyad imputation
uncertainty through to the final models.

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
spatial-weights and intervention-coding stages.  Model estimates are combined
across datasets using Rubin's Rules.

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

Row-normalisation gives $W_(i j) = w_(i j) / (sum_k w_(i k))$, so that each row
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
intervener $B$ is computed as the weighted sum of other potential interveners'
intervention choices, with $A$ removed from the weight matrix before
computation:

$ "spat_gov"_B = sum_(j eq.not A, j eq.not B) W_(B j) dot bb(1)[y_j = 1] $

where $y_j in {0, 1, 2}$ is the intervention code of state $j$ in this onset
($1$ = government-biased, $2$ = opposition-biased).  An analogous expression
gives `spat_opp`.
