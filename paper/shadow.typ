// ============================================================
// Civil War in the Shadow of Intervention
// ============================================================

#import "style.typ": *

#show: paper.with(
  title:    [Civil War in the Shadow of Intervention],
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
    with this measure, I find that intervention expectations improve both
    in-sample fit and out-of-sample predictive performance, with the
    strongest gains after the Cold War's end.
    // TODO: update once empirical results are finalised.
  ],
)

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
