// ============================================================
// Introduction
// ============================================================

= Introduction

Third-party military intervention in civil wars is typically studied as a
response: states observe an ongoing conflict and decide whether, and on
whose behalf, to send troops.  Intervention also operates as a cause.
Well before the first shots are fired, potential opposition groups form
expectations about the international environment they would face should
they decide to fight.  If powerful states are likely to back the
government, the expected return to rebellion falls.  If outside powers
are likely to support the opposition, the expected return to rebellion
rises.  These anticipatory calculations---the shadow of
intervention---shape the decision to initiate civil war.  This
theoretical prediction is well-established @cetinyan2002
@cunningham2016 @lango2023 @sambanis2020.  Recent structural work derives such
expectations from the equilibrium of a game among the five permanent
members of the UN Security Council @gibilisco2022.  What has been missing is a
measure covering the full population of potential interveners, varying
annually with shifting alliance portfolios and foreign policy
alignments, and treating intervention direction---government-biased
versus opposition-biased---as the primary outcome rather than an
extension.  This paper constructs one.

The central contribution is a country-year measure of the expected
intervention environment.  Using the #cite(<regan2000>, form: "prose") dataset of military
interventions in civil conflicts (1946--1999), I train a machine-learning
classifier to predict, for each directed dyad (potential intervener $B$,
civil war host $A$) in a given year, the probability that $B$ sends
military forces and the side it would favor.  The classifier draws on a
rich feature set covering the material and political characteristics of
potential interveners, the bilateral relationship between intervener and
host, foreign policy alignment, host-country conditions, and the spatial
pattern of other states' intervention choices.  Dyad-level predicted
probabilities are then aggregated across all potential interveners to
produce a country-year measure of the total expected intervention
environment---the shadow.

The construction is disciplined by a simple game-theoretic model in which
an opposition group chooses whether to fight given rational expectations
over the intervention decisions of up to $n$ potential outside powers.
The model does not establish that intervention expectations matter---that
is the prior literature's contribution---but clarifies the form those
expectations must take: a weighted sum of intervener-specific
probabilities and utilities in which the strategies of potential
interveners are mutually consistent.  This consistency requirement, a
Nash fixed-point condition, is built directly into the measure.

I validate the measure by testing whether it predicts civil war onset.
Following the country-year framework of #cite(<fearon2003>, form: "prose"), I estimate logistic
models of onset augmented by the expected intervention environment.
Expected government-biased intervention enters with a negative
coefficient, consistent with deterrence, while expected
opposition-biased intervention enters positively,
consistent with emboldening---both sides of the shadow operating exactly
as the theory predicts.  The directional pattern is robust across all
25 measurement draws, nine extended specifications, and a country
fixed-effects estimator.  Five of the ten specifications achieve positive
out-of-sample proportional reduction in log-loss, confirming that the
shadow variables carry genuine predictive content rather than in-sample
noise.  Controlling for intervention
expectations attenuates the coefficient on log population by 47--83%,
suggesting that a substantial share of the "large country" risk
identified in the macro-correlates literature is in fact a consequence of
the richer strategic environment that large states generate.

The macro-correlates approach to civil war @fearon2003 has identified a set
of structural risk factors---low income, rough terrain, ethnic cleavages, weak state
capacity---but most of these factors evolve slowly, limiting their ability
to explain the timing of onset.  Intervention expectations, which ebb and
flow with the shifting international environment, vary in ways that can
explain why violence erupts in some years but not others in already-risky
countries.  The paper extends a line of theoretical work initiated by #cite(<cetinyan2002>, form: "prose"),
who models potential conflict between an ethnic group, a government, and a
single potential intervener, and developed by #cite(<cunningham2016>, form: "prose"), who
examines how the prospect of superpower government-biased intervention
suppresses civil violence.  The measure generalizes these frameworks in
three directions: by addressing both government- and opposition-biased
intervention; by modeling the potential intervention decisions of every
state in the international system rather than superpowers alone; and by
allowing for contagion among interveners through a spatial weighting
scheme.

The paper is most closely related to #cite(<gibilisco2022>, form: "prose"),
who derive equilibrium intervention expectations from a structural game
among the P5 powers and estimate them via maximum simulated likelihood.
The structural approach offers clean counterfactual predictions and
explicit identification of strategic spillovers among the most consequential
actors---advantages that require restricting the player set to a size that
keeps joint-game estimation tractable.  This paper is best understood as
complementary.  The Regan data record military interventions by 67 distinct
states; the five permanent Security Council members account for fewer than
half of all intervention events, while the remaining 61 intervening
states---neighbors containing conflict spillover, coethnics protecting kin,
Cold War proxies following patron signals, regional rivals
balancing---represent the majority of actual deployments and are driven by
strategic logics qualitatively different from the global-order interests
that animate P5 behavior.  A structural game pre-specified over the P5
cannot recover these dynamics; the data must determine who matters.  The
Nash fixed-point condition replaces the joint-game equilibrium with a
minimal consistency requirement---each state's predicted probability must be
self-consistent as an input to the others' predictions---without specifying
the full game among all $n$ players.

The connection to the structural approach runs deeper at the estimation
stage.  The unpenalised multinomial logistic regression---a softmax
best-response function---is one of nine component learners in the Stage 1
ensemble, directly nesting the functional form that
#cite(<gibilisco2022>, form: "prose") assume.  Its weight in the final stack
is determined entirely by out-of-fold predictive performance: where the
structural functional form captures the dominant behavioral pattern, it
earns weight; where more flexible learners fit better, they prevail.

The two designs also differ in time structure.  #cite(<gibilisco2022>,
form: "prose") produce cross-sectional estimates averaged over 1950--1999;
the present approach produces annual estimates, enabling longitudinal
analysis of onset _timing_ within countries at elevated structural risk.
Direction of intervention---government-biased versus
opposition-biased---is the primary dependent variable, not an appendix.

On the measurement side, the paper joins a growing body of work that
brings machine learning to the prediction of conflict and to the
construction of latent quantities for use in structural models
@carroll2019 @ward2010.  The key methodological contribution is a two-stage
strategy in which a first-stage ensemble predicts intervention
probabilities that are then entered into a second-stage onset model,
preserving the tight link between theoretical content and empirical
measurement.  The two-stage logic has a precedent in the civil war
literature: #cite(<akcinaroglu2005>, form: "prose") predict rival intervention probability in a
first stage and show that the resulting expectations---not realized
intervention---prolong conflict duration.  #cite(<rubinmalone2024>, form: "prose") reinforce the
theoretical emphasis on anticipated rather than realized intervention: at
the armed-group level, realized foreign sponsorship predicts civil war
escalation no better than the organizational characteristics that
simultaneously attract outside support, consistent with the strategic
shadow of intervention---not its realization---doing the causal work.
The present paper extends this approach to onset and to the full
population of potential interveners.

The paper makes two contributions.  The first is the measure itself:
the first country-year, direction-disaggregated series of the expected
intervention environment covering the full population of potential
interveners, varying annually with shifting alliance portfolios and
foreign policy alignments.  Existing measures either aggregate across
direction, restrict the intervener set to great powers, or fix
expectations cross-sectionally.  The shadow does none of these.  It is
also constructed with deliberate theoretical discipline: the
dyad-level predicted probabilities that feed into the aggregate are
required to satisfy a Nash fixed-point condition, so the expectations
embedded in the measure are mutually consistent across potential
interveners---an equilibrium requirement that #cite(<gibilisco2022>,
form: "prose") enforce within the P5 and that the present approach
extends to the full intervener population without specifying the joint
game among all $n$ players.

The second contribution is empirical.  Intervention expectations
predict the timing of civil war onset, not merely which countries face
elevated structural risk.  Macro-correlates of conflict---income,
terrain, ethnic cleavages---evolve slowly and cannot explain why
violence erupts in some years but not others in already-risky
countries.  The shadow, which fluctuates with the international
environment, can.

The paper proceeds as follows.  @sec-motivation develops the theoretical
framework.  @sec-constructing describes the measure of intervention
expectations: the outcome variable, feature set, and classifier design.
The onset analysis appears in @sec-decision.
