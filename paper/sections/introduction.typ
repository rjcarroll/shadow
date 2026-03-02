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
@cunningham2016; what has been missing is a direct measure of the
expected intervention environment that is faithful to the theory's causal
structure.  This paper constructs one.

The central contribution is a country-year measure of the expected
intervention environment.  Using the @regan2000 dataset of military
interventions in civil conflicts (1946--1999), I train a machine-learning
classifier to predict, for each directed dyad (potential intervener $B$,
civil war host $A$) in a given year, the probability that $B$ sends
military forces and the side it would favour.  The classifier draws on a
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
Following the country-year framework of @fearon2003, I estimate logistic
models of onset augmented by the expected intervention environment.
// TODO: preview key findings once empirical results are available.

These results speak to several active literatures.  The macro-correlates
approach to civil war @fearon2003 has identified a set of structural
risk factors---low income, rough terrain, ethnic cleavages, weak state
capacity---but most of these factors evolve slowly, limiting their ability
to explain the timing of onset.  Intervention expectations, which ebb and
flow with the shifting international environment, vary in ways that can
explain why violence erupts in some years but not others in already-risky
countries.

The paper extends a line of theoretical work initiated by @cetinyan2002,
who models potential conflict between an ethnic group, a government, and a
single potential intervener, and developed by @cunningham2016, who
examines how the prospect of superpower government-biased intervention
suppresses civil violence.  The measure generalises these frameworks in
three directions: by addressing both government- and opposition-biased
intervention; by modelling the potential intervention decisions of every
state in the international system rather than superpowers alone; and by
allowing for contagion among interveners through a spatial weighting
scheme.

On the measurement side, the paper joins a growing body of work that
brings machine learning to the prediction of conflict @ward2010 and to the
construction of latent quantities for use in structural models
@carroll2019.  The key methodological contribution is a two-stage
strategy in which a first-stage ensemble predicts intervention
probabilities that are then entered into a second-stage onset model,
preserving the tight link between theoretical content and empirical
measurement.

The paper proceeds as follows.  @sec-motivation develops the theoretical
framework.  @sec-constructing describes the measure of intervention
expectations: the outcome variable, feature set, and classifier design.
The onset analysis appears in @sec-decision.
The conclusion follows.
