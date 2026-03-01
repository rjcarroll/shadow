# Civil War in the Shadow of Intervention

This repository contains the data pipeline, models, and paper for an updated and
extended version an old working paper entitled _Civil War in the Shadow of Intervention_.

The core argument: expectations of third-party intervention shape opposition groups'
decisions to start a civil war. The paper builds a predictive measure of intervention
expectations using an ensemble machine learning approach, then tests whether those
expectations improve our ability to predict civil war onset.

## What's new in this version

- **Extended coverage** — data runs through ~2023, using UCDP/PRIO instead of
  Fearon and Laitin (2003) for civil war onset
- **Python** — the full pipeline is rewritten from R to Python
- **Streamlined theory** — the formal game-theoretic model is condensed; the
  empirical sections are the focus
- **Richer Stage 2** — logistic regression remains the main onset model, with
  random forest and gradient boosting robustness checks

## Repository layout

```
data/           Source data (not tracked; see data/README.md for downloads)
src/shadow/     Installable Python package with data and model code
notebooks/      Jupyter notebooks — one per pipeline stage
tests/          pytest test suite
paper/          Typst source for the paper
results/        Generated figures and tables (not tracked)
```

## Setup

```bash
pip install -e ".[dev]"
```

## Pipeline

Run the notebooks in order:

| Notebook | Stage |
|---|---|
| `01-country-year.ipynb` | Build country-year dataset |
| `02-dyad-data.ipynb` | Expand to directed dyads |
| `03-interventions.ipynb` | Code intervention outcomes |
| `04-spatial-weights.ipynb` | Build spatial weights matrix |
| `05-stage1-training.ipynb` | Train ML ensemble (Stage 1) |
| `06-stage1-predictions.ipynb` | Generate intervention expectations |
| `07-stage2-onset.ipynb` | Onset logit models (Stage 2) |
| `08-robustness.ipynb` | ML robustness checks for Stage 2 |
| `09-figures.ipynb` | All paper figures |

## Tests

```bash
pytest tests/
```
