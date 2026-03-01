// ============================================================
// Civil War in the Shadow of Intervention
// ============================================================

#import "style.typ": *

#show: paper.with(
  title:    [Civil War in the Shadow of Intervention],
  author:   [Robert J. Carroll],
  abstract: [
    I argue that expectations of third-party intervention shape the
    decision to start a civil war. // TODO
  ],
)

#include "sections/introduction.typ"
#include "sections/motivation.typ"
#include "sections/constructing.typ"
#include "sections/decision.typ"
#include "sections/conclusion.typ"

#bibliography("references.bib", style: "chicago-author-date")
