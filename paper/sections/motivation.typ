// ============================================================
// Intervention and the Onset Decision
// ============================================================

= Intervention and the Onset Decision <sec-motivation>

The conventional treatment of third-party intervention studies it as a
consequence of civil war: states respond to conflicts that have already
begun, bargaining to end them or tilting the balance toward a preferred
side.  I argue instead that intervention is also a cause of civil war.
The prospect of outside military assistance---or the threat of outside
resistance---shapes the incentive calculus of potential opposition groups
before a single shot is fired.  Civil war onset, in this view, happens
in the shadow of intervention.

== A Simple Model

Consider the decision problem depicted in @fig-ntree.  An opposition
group $O$ begins the game by choosing whether to Fight ($s_O = F$) or
Not fight ($s_O = N$).  Should the opposition choose not to fight, the
game ends at the status quo $S Q$.

#figure(
  image("../figures/ntree.pdf", width: 85%),
  caption: [Motivating formal model.],
) <fig-ntree>

If the opposition chooses to fight, the game moves to an intervention
subgame in which a set of $n$ potential interveners make an
interdependent decision on whether to intervene and, crucially, on whose
behalf.  Each potential intervener $i$ selects $s_i in {O, N, G}$:
opposition-biased ($O$), non-intervention ($N$), or government-biased
($G$).  The set of possible intervention profiles is $S = {O, N, G}^n$.

Each potential intervener $i$ is equipped with a utility function
$u_i : S arrow.r RR$.  Working under expected utility, let $sigma_i$
be a probability measure over ${O, N, G}$, and let
$U_i (sigma) = sum_(s' in S) u_i (s') product_i sigma_i (s'_i)$.
A Nash equilibrium $sigma^*$ satisfies
$U_i (sigma_i^*, sigma_(-i)^*) >= U_i (sigma'_i, sigma_(-i)^*)$
for every $sigma'_i$ and every $i$.

The equilibrium intervention profile $sigma^*$ feeds back to the
opposition group.  The opposition's expected utility of fighting is
$U_O (F; sigma^*) = sum_(s' in S) u_O (s') product_i sigma_i^* (s'_i)$,
where $u_O (s') in RR$ is their payoff under profile $s'$.  A subgame
perfect equilibrium pair $(sigma^*, s_O^*)$ requires that $sigma^*$ be
a Nash equilibrium of the intervention subgame and that

$ u_O (S Q) >= U_O (F; sigma^*) <==> s_O^* = N. $

The opposition chooses not to fight if and only if the status quo utility
at least matches their expected utility of fighting given equilibrium
intervention.  This is the shadow of intervention: the anticipated
behaviour of outside powers shapes whether the opposition mobilises at
all.

== Simplifying Assumptions

The equilibrium condition above is theoretically clean but empirically
intractable as stated.  With up to 193 potential interveners each
choosing among three options, the number of possible profiles reaches
$3^(192)$---larger than the number of elementary particles in the
observable universe.  I adopt two assumptions to reduce the problem to a
manageable form.

*Assumption 1 (Separability).*  The opposition's payoff from any profile
is the sum of each intervener's individual contribution:
$u_O (s') = sum_i u_O^i (s'_i)$, where $u_O^i : {O, N, G} arrow.r RR$
measures intervener $i$'s marginal effect on opposition utility under
action $s'_i$.  Separability allows the opposition to evaluate each
potential intervener's likely behaviour independently rather than over
the full combinatorial space.

The assumption is strong but yields a useful property: each intervener
makes a unique, identifiable contribution to opposition utility.
Tractability comes at some cost.  If the US and UK both intervene for
the opposition, the model records the combined effect as the sum of the
two marginal utilities, allowing for neither synergies nor diminishing
returns.  This limitation is partially addressed through functional-form
choices in the empirical analysis below.

*Assumption 2 (Non-intervention as reference).*  For all $i$,
$u_O^i (N) = 0$: non-intervention has no effect on opposition utility.
This normalization implies that if no state intervenes, the expected
utility of fighting reduces to any purely domestic utility; the
standard, intervention-free model of civil war onset is therefore a
limiting case.

Under these two assumptions, the opposition's expected utility of
fighting simplifies to

$ U_O (F; sigma) = sum_(i=1)^n [sigma_i (O) u_O^i (O) + sigma_i (G) u_O^i (G)]. $ <eq-EU>

@eq-EU is the primary theoretical object.  The empirical strategy is to
estimate the four quantities on the right-hand side: first the
intervention probabilities $sigma_i (O)$ and $sigma_i (G)$ for each
potential intervener using the Stage 1 classifier described in
@sec-constructing, and then the utility parameters $u_O^i (O)$ and
$u_O^i (G)$ using a second-stage onset model.

== Scope and Structure

A few important qualifications deserve mention before proceeding.

The model assigns the strategic decision to the opposition group.  This
is not a claim that governments play no role; it reflects the literature's
longstanding focus on the conditions that facilitate or inhibit
insurgency @fearon2003 and the practical convenience of modelling the
actor who decides whether to initiate violence.  Governmental decisions
enter the model implicitly through the opposition's status quo utility
and through the host-country covariates in the classifier.  Results are
therefore interpretable from either side: a finding that government-biased
major-power intervention suppresses onset could reflect opposition groups
anticipating a crushing defeat, or governments behaving more brutally when
they enjoy credible external backing.  The data cannot distinguish these
two channels.

The model also restricts attention to intervention by states.  This
greatly simplifies both theory and estimation: states have observable
material capabilities, regime types, alliance portfolios, and foreign
policy positions that can be used as predictors.  International
organisations could in principle be incorporated as a fourth intervener
option---buckpassing to the United Nations---but this extension is left
for future work.

Finally, the model abstracts away from the bargaining interaction between
the opposition and the government that underlies most formal treatments of
civil war @fearon1995.  This is deliberate.  It is not obvious
which bargaining game is appropriate, and adding another layer of strategic
uncertainty would multiply the required assumptions without obviously
improving empirical tractability.  The goal is a model with a tight
deductive relationship to its empirical counterpart, not a fully
microfounded account of the civil war process.
