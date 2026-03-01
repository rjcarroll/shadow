"""Code the intervention outcome variable.

Replaces R script 08-addInterventions.R.

Maps Regan (2000) intervention records onto directed-dyad-year observations.
Each dyad-year with a civil war onset is coded as:
  0 = no intervention
  1 = government-biased intervention
  2 = opposition-biased intervention

For post-1999 coverage, candidates include:
  - UCDP External Support Dataset (Högbladh et al.)
  - ACLED (Armed Conflict Location and Event Data)

Output: adds `intervention` column to interim dyad files
"""
