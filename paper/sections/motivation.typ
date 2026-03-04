// ============================================================
// Intervention and the Onset Decision
// ============================================================

= Intervention and the Onset Decision <sec-motivation>

What does it mean, formally, for an opposition group to act in the shadow
of intervention?  This section develops a model of the onset decision that
derives the functional form that intervention expectations must take to be
both theoretically coherent and empirically tractable.

== The Model

#figure(
  image("../figures/ntree.pdf", width: 85%),
  caption: [Motivating formal model.],
) <fig-ntree>

The decision problem is depicted in @fig-ntree.  An opposition group $O$
chooses whether to Fight ($s_O = F$) or Not fight ($s_O = N$).  Should it
not fight, the game ends at the status quo $S Q$.  If it fights, $n$
potential interveners simultaneously choose actions $s_i in {O, N, G}$---
opposition-biased, non-intervention, or government-biased---and the
combined profile $s in S = {O, N, G}^n$ determines the international
environment the opposition faces.

Each potential intervener $i$ has a utility function $u_i : S arrow.r RR$
over profiles.  Under expected utility, let $sigma_i$ be a probability
measure over ${O, N, G}$, and let
$U_i (sigma) = sum_(s' in S) u_i (s') product_j sigma_j (s'_j)$.
A Nash equilibrium $sigma^*$ of the intervention subgame satisfies
$U_i (sigma_i^*, sigma_(-i)^*) >= U_i (sigma'_i, sigma_(-i)^*)$
for every $sigma'_i$ and every $i$.  The equilibrium profile feeds back
to the opposition, whose expected utility of fighting is
$U_O (F; sigma^*) = sum_(s' in S) u_O (s') product_i sigma_i^* (s'_i)$.
A subgame perfect equilibrium requires that $sigma^*$ be a Nash equilibrium
of the intervention subgame and that

$ u_O (S Q) >= U_O (F; sigma^*) <==> s_O^* = N. $

The opposition fights if and only if the expected return to fighting, given
equilibrium intervention, exceeds the status quo.  The international
environment enters the calculus entirely through $sigma^*$: the anticipated
behavior of outside powers can make the difference between peace and war
before a single shot is fired.  This is the shadow of intervention
@cetinyan2002.#footnote[
  Whether $sigma^*$ approximates the expectations that rebels actually form
  is not merely assumed.  Under rational expectations, agents exploit
  available information at least as well as the researcher's model
  @muth1961.  The Stage 1 classifier described in @sec-constructing
  recovers a substantial share of the predictive content in realized
  intervention outcomes using only publicly observable state
  characteristics; rebels who can also draw on private intelligence,
  direct patron relationships, and local knowledge face a forecasting
  problem that is easier, not harder, than the one the classifier solves.
]

== From Equilibrium to Empirics

The equilibrium condition is theoretically exact but empirically intractable
as stated.  With up to 193 potential interveners each choosing among three
options, the expectation over $U_O$ runs over $3^(192)$ profiles---larger
than the number of elementary particles in the observable universe.  Two
assumptions reduce the problem to a tractable form without discarding its
theoretical content.

*Assumption 1 (Separability).*  The opposition's payoff from any profile
is additive across interveners:
$u_O (s') = sum_i u_O^i (s'_i)$, where $u_O^i : {O, N, G} arrow.r RR$
measures the marginal contribution of intervener $i$'s action.

Separability captures a specific and substantively defensible theory of how
opposition groups aggregate international expectations: each outside power's
likely behavior enters the calculus independently, and the combined effect
is their sum.  A rebel coalition assessing its international environment in
1970s Angola had a view about what Cuba would likely do, a separate view
about South Africa, and a separate view about the United States; the
hypothesis is that these entered additively.  The assumption rules out
complementarities---the value of Soviet logistics may depend on whether
Cuban troops are also present---but such complementarities require
coordinated multilateral operations of a kind that constitute a small
minority of actual interventions.  Richer utility specifications in Stage 2
partially relax the additivity constraint through interaction terms.

Separability also enables the extension to the full system of potential
interveners.  Because each state's contribution enters @eq-EU additively,
computing the opposition's expected utility requires only the marginal
intervention probabilities $sigma_i$---not the joint distribution over all
$n$ players' actions simultaneously.  Specifying and estimating the full
joint game---the approach of #cite(<gibilisco2022>, form: "prose") for the
five permanent Security Council members---becomes computationally
intractable beyond a small player set.  Separability sidesteps that
constraint while preserving the game's central implication: the equilibrium
strategies of outside powers must be mutually consistent.  That consistency
requirement is enforced directly via the Nash fixed-point iteration
described in @sec-constructing.

*Assumption 2 (Non-intervention as reference).*  For all $i$,
$u_O^i (N) = 0$.  This normalization makes the intervention-free onset
model a limiting case: if no state intervenes, the expected utility of
fighting reduces to purely domestic considerations, and the standard
country-year onset model follows directly.

Under these two assumptions, the expected utility of fighting simplifies to

$ U_O (F; sigma) = sum_(i=1)^n [sigma_i (O) u_O^i (O) + sigma_i (G) u_O^i (G)]. $ <eq-EU>

@eq-EU is the primary theoretical object, and the two-stage empirical design
follows directly from its structure.  The equation contains two types of
unknowns: the intervention probabilities $sigma_i (O)$ and $sigma_i (G)$,
and the marginal utilities $u_O^i (O)$ and $u_O^i (G)$ that scale each.
Stage 1 (@sec-constructing) estimates the probabilities for every directed
dyad using a machine-learning classifier trained on the Regan intervention
record.  Stage 2 (@sec-decision) recovers the utility parameters from their
aggregate effect on civil war onset.  The theory does not merely motivate
the empirical strategy; it specifies exactly what each stage must estimate
and how the estimates connect.

The model assigns the strategic decision to the opposition, not because
governments are passive but because this is where the onset decision
sits---following the literature's longstanding focus on the conditions that
facilitate or inhibit insurgency @fearon2003.  Governmental behavior enters
implicitly through the opposition's status quo utility and the host-country
features in the Stage 1 classifier.  A richer treatment would endogenize the government's response, but
specifying and estimating a bargaining game would sever the tight deductive
connection between @eq-EU and its empirical counterpart---the connection
that makes the two-stage design interpretable rather than merely convenient.
