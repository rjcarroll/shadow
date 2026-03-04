# Fighting in the Shadow of Intervention: A Learned-Proxy Analysis

Replication archive for "Fighting in the Shadow of Intervention: A Learned-Proxy Analysis."

Expectations of third-party military intervention shape the decision to start
a civil war. This paper builds a learned proxy for those expectations using a
super-learner ensemble trained on directed-dyad data from 190 civil war onsets
(1946--2014), then tests whether the resulting measure --- the *shadow of
intervention* --- predicts civil war onset in a standard logit framework.
Measurement-stage uncertainty is propagated into the second-stage standard
errors following Knox, Lucas, and Cho (2022).

## Repository layout

```
src/shadow/         Installable Python package
  data/               Data construction modules (ccode, country_year, dyad,
                        impute, interventions, spatial)
  models/             Ensemble learner and onset models (ensemble, onset)
notebooks/          Jupyter notebooks --- one per pipeline stage (see below)
scripts/            Standalone scripts (bootstrap, batch re-run)
paper/              Typst source for the paper
  sections/           Section files (introduction, motivation, constructing,
                        decision, conclusion, appendix)
  figures/            Generated PDFs
data/               Source data (not tracked; see data/README.md)
tests/              pytest test suite
```

## Setup

Requires Python 3.12+ and libomp (`brew install libomp` on macOS, needed
by LightGBM/miceforest).

```bash
python -m venv .venv
source .venv/bin/activate
pip install -e ".[dev]"
```

## Pipeline

Run the notebooks in order. Each reads from `data/interim/` and writes back
to it; final outputs go to `data/interim/` (model files, predictions) and
`paper/figures/` (PDFs).

| Notebook | Stage | Time |
|---|---|---|
| `01-country-year` | Build country-year panel (Polity, NMC, trade, ideal points) with multiple imputation (5 draws) | ~5 min |
| `02-dyad-data` | Expand to 25 directed-dyad files (5 CY x 5 UD imputations) with alliance, contiguity, DOE, IGO, ethnic, rivalry features | ~45 min |
| `03-interventions` | Merge Regan (1944--1999) + hand-coded post-1999 interventions; 461 intervention-dyad records | ~1 min |
| `04-spatial-weights` | Build spatial weight matrices from intervention predictions | ~20 min |
| `05-stage1-training` | Train super-learner ensemble (9 classifiers, PCA, 10-fold CV, NNLS stacking) x 25 draws | ~35 hrs |
| `06-stage1-predictions` | Generate shadow measure: aggregate dyad predictions to country-year E_gov / E_opp | ~30 min |
| `07-stage2-onset` | Onset logit (9 specs), T x P bootstrap (KLC 2022), FE, robustness checks | ~4 hrs |
| `09-figures` | All paper figures (7 PDFs) | ~5 min |

For batch re-runs: `caffeinate -i bash scripts/rerun_extended.sh 2>&1 | tee rerun.log`

## Stage 1: Super-learner ensemble

Nine component classifiers spanning three families:

- **Trees:** random forest (500 trees), histogram-gradient boosting (lr=0.10), histogram-gradient boosting (lr=0.05)
- **Logistic:** ridge, elastic net, LASSO, unpenalized multinomial
- **Neural:** MLP (25 units), MLP (100, 50 units)

PCA reduces ~105 features to 54 components (90% cumulative variance).
NNLS stacking combines out-of-fold predictions. Typical ensemble weights:
RF ~50%, MLP(100,50) ~30%, HGB(lr=0.05) ~15%.

Out-of-fold performance (Regan period): PRL ~45%, AUC ~0.965.

## Stage 2: Onset logit with T x P bootstrap

The shadow measure (E_gov, E_opp) enters a country-year onset logit alongside
polity, instability, GDP, population, and (in richer specifications) entrant
counts, major-power indicators, and contiguous-neighbor variables. Standard
errors are corrected for generated-regressor uncertainty by bootstrapping over
both imputation draws (T=25) and cluster-resampled observations (P=200),
following Knox, Lucas, and Cho (2022, Section 4.2).

## Tests

```bash
pytest tests/
```

## AI disclosure

This project was developed extensively using Claude Code (Anthropic, Claude
Opus 4, January--March 2026). Claude Code wrote the Python pipeline, implemented
the statistical methods, and assisted with manuscript drafting. The author
directed all research design decisions, coding judgments, and substantive
interpretations. See the cover letter for full details. All code is
deterministic and can be audited independently of its authorship.
