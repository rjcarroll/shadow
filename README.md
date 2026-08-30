# Fighting in the Shadow of Intervention: A Learned-Proxy Analysis

Research code for the paper, conditionally accepted at *Political Science
Research and Methods* (August 2026). A replication package for the PSRM
Dataverse is in preparation; see [`REPLICATION.md`](REPLICATION.md).

Expectations of third-party military intervention shape the decision to
start a civil war — but the quantity that matters is an expectation, not a
realized event, and existing proxies are either static or restricted to the
P5. This project builds a *learned proxy* for the shadow of intervention in
two stages. **Stage 1** trains a super-learner ensemble on directed
dyad-years (1946–2014) to predict the probability and direction
(government- vs. opposition-biased) of intervention, imposes a Nash
fixed-point condition so that predictions are self-consistent inputs to
their own spatial features, and aggregates to country-year shadow measures
$E^G$ and $E^O$. **Stage 2** asks whether the shadow enters the onset
calculus as theory predicts, propagating measurement uncertainty through a
T×P bootstrap and reading direction through a net-tilt / common-intensity
reparameterization gated by out-of-sample prediction.

This README deliberately reports no statistics. Every number, table, and
figure in the paper is generated from pipeline outputs (see below); results
live there, not here, so they cannot drift.

## Exhibits by construction

- [`scripts/export_numbers.py`](scripts/export_numbers.py) is the single
  source of truth for every reported statistic: it writes
  `paper/generated/numbers.tex` (one `\newcommand` per in-prose number) and
  every generated table in `paper/tables/`.
- [`scripts/fig_shadow_ts.py`](scripts/fig_shadow_ts.py) and
  [`scripts/fig_appendix.py`](scripts/fig_appendix.py) produce the paper's
  data figures.
- The manuscript `\input`s these files, so the page equals the pipeline
  output by construction. Verified against the accepted manuscript:
  regeneration is byte-identical, and a seeded 25-item random audit of
  reported values reproduced 25/25.

## Repository layout

```text
src/shadow/         Installable Python package
  data/               Data construction (ccode, country_year, dyad,
                        impute, interventions, spatial)
  models/             Ensemble learner and onset models
  utils/              Plotting utilities
notebooks/          Pipeline stages 01–07, 09 (see below)
scripts/            Exhibit generators, out-of-sample gates, direction and
                      channel analyses, predictive-significance tests,
                      diagnostics, and batch runners
paper/              LaTeX source
  sections/           Section files
  generated/          numbers.tex — every reported statistic as a macro
  tables/             Generated booktabs tables
  figures/            Generated PDFs (+ TikZ sources)
docs/               Portfolio overview (Quarto)
data/               Source data (not tracked; see data/README.md)
tests/              pytest test suite
REPLICATION.md      Replication package design (PSRM Dataverse)
```

## Setup

Requires Python ≥ 3.11 (developed on 3.14) and libomp
(`brew install libomp` on macOS; needed by LightGBM/miceforest).

```bash
python -m venv .venv
source .venv/bin/activate
pip install -e ".[dev]"
```

## Pipeline

Notebooks run in order; each reads from and writes to `data/interim/`.
Stage-1 training is the heavy step (order of a day or more on a laptop);
everything else runs in minutes to hours.

| Notebook | Stage |
| --- | --- |
| `01-country-year` | Country-year panel with multiple imputation (5 draws) |
| `02-dyad-data` | Directed-dyad expansion (5 × 5 = 25 complete datasets) |
| `03-interventions` | Intervention coding: Regan (1944–1999) + hand-coded post-1999 extension |
| `04-spatial-weights` | Spatial weight matrices, one per imputation draw |
| `05-stage1-training` | Super-learner training (9 learners × 3 feature sets per draw, NNLS stacking) |
| `06-stage1-predictions` | Universal predictions, Nash fixed point, aggregation to $E^G$/$E^O$ |
| `07-stage2-onset` | Onset models, T×P bootstrap, robustness |
| `09-figures` | Exploratory figures (paper figures come from `scripts/`) |

The extended analyses behind Section 3 and the appendix (leave-one-country-out
gates, subsumption, direction and channels, drop-column importance,
predictive-significance tests, fixed-point diagnostics) live in `scripts/`,
with `run_*.sh` batch runners.

## Tests

```bash
pytest tests/
```
