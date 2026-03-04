// ============================================================
// Fighting in the Shadow of Intervention: A Learned-Proxy Analysis
// ============================================================

#import "style.typ": *

#show: paper.with(
  title:    [Fighting in the Shadow of Intervention: A Learned-Proxy Analysis],
  author:   [Robert J. Carroll],
  abstract: [
    Opposition groups decide whether to fight against a backdrop of
    international expectations: which outside powers might intervene, on
    whose behalf, and with what probability.  Theory predicts these
    anticipated decisions---the shadow of intervention---shape the
    opposition's calculus before the first shot is fired, yet direct
    measures of the expected intervention environment have been absent from
    civil war onset research.  I construct such a measure using a
    machine-learning classifier trained on the @regan2000 dataset of
    military interventions (1946--1999).  The classifier combines over 30
    directed-dyad-year features---covering potential interveners' material
    and political characteristics, bilateral ties, foreign policy
    alignment, and the spatial pattern of other states' choices---within a
    super-learner ensemble that produces calibrated probability estimates
    for government- and opposition-biased intervention.  Predicted
    probabilities are aggregated to a country-year measure of the expected
    intervention environment, imposing the self-consistency condition that
    equilibrium strategies be mutually coherent across potential
    interveners.  Augmenting a standard logistic model of civil war onset
    with this measure, I find that expected government-biased intervention
    deters onset while expected opposition-biased intervention
    encourages it---a directional pattern that is robust across all
    measurement draws, nine extended specifications, and a country
    fixed-effects estimator.  The coefficient on log population---widely considered
    the most robust correlate of civil war---attenuates by 47--83% once
    intervention expectations are controlled for.  The shadow measure
    completely subsumes the explanatory power of the Lake security
    hierarchy used by @cunningham2016, confirming that the richer feature
    set and non-superpower coverage add substantial information beyond
    the patron-client channel alone.
  ],
)

// ── Acknowledgments ───────────────────────────────────────────────────────────
//#heading(level: 1, numbering: none)[Acknowledgments]

//This paper has benefitted from discussions with Jeff Arnold,
//Quintin Beazer, Inken von Borzyskowski, Mike Colaresi, Tyson Chatagnier, Kevin
//Clarke, Casey Crisman-Cox, Amanda Driscoll, Kevin Fahey, Mark Fey, Nisha Fazal,
//Hein Goemans, Phil Henrickson, Gary Hollibaugh, Brenton Kenkel, Hye-Sung Kim,
//Bethany Lacina, Jeff Marshall, Will Moore, Jonathan Olmsted, Kai Ou, Matt
//Pietryka, Amy Pond, Pat Regan, Chris Reenock, Curt Signorino, Brad Smith, Mark Souva, Randy
//Stone, Susanna Supalla, and Jaroslav Tir.  The usual caveat applies.

#include "sections/introduction.typ"
#include "sections/motivation.typ"
#include "sections/constructing.typ"
#include "sections/decision.typ"
#include "sections/conclusion.typ"



#bibliography("references.bib", style: "chicago-author-date")

// ── Online Appendix ───────────────────────────────────────────────────────────
#pagebreak()
#align(center)[
  #v(1em)
  #text(size: 14pt, weight: "bold")[Online Appendix]
  #v(1em)
]

#include "sections/appendix.typ"
