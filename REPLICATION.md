# Replication Package — *Fighting in the Shadow of Intervention*

Replication materials for "Fighting in the Shadow of Intervention: A
Learned-Proxy Analysis" (*Political Science Research and Methods*,
PSRM-2026-0115). Contact: Robert J. Carroll.

## 1. What to run

One command regenerates every number, table, and figure in the article and
online appendix from the data shipped in this package, compares the results
against the shipped (published) versions, and writes a timestamped log:

```bash
bash scripts/run_replication.sh
```

Runtime: **under one minute.** Expected final lines:

```text
RESULT: SUCCESS — every regenerated number and table is
byte-identical to the shipped (published) versions, and every
figure matches up to its embedded creation timestamp.
```

Exit status 0 means every regenerated exhibit matched: LaTeX numbers and
tables byte-for-byte, figure PDFs up to the few bytes of their embedded
creation timestamp. The log file (`replication-YYYYMMDD-HHMMSS.log`) is the
record of that comparison. The manuscript `\input`s the generated files —
`paper/generated/numbers.tex` (every in-prose statistic as a macro) and
`paper/tables/*.tex` (every table) — so a successful run demonstrates that
the published pages equal the pipeline output by construction. Optionally,
`latexmk paper/shadow.tex` then recompiles the manuscript itself from the
regenerated inputs.

## 2. Environment

Python ≥ 3.11. The results were produced with Python 3.14.4 on macOS
(Apple silicon); the exact package versions are in `requirements-freeze.txt`.

```bash
python -m venv .venv
source .venv/bin/activate
pip install -r requirements-freeze.txt
pip install -e .           # the shadow package itself (src/shadow)
```

`brew install libomp` (macOS) is needed only for the full rebuild
(LightGBM, used by the imputation library) — not for Section 1's
verification run.

## 3. What produces what

| Manuscript exhibit | Producer | Generated file |
| --- | --- | --- |
| Every in-prose statistic | `scripts/export_numbers.py` | `paper/generated/numbers.tex` |
| All 11 statistical tables (Stage-1 performance, ensemble components, interveners, shadow dyads, OOS gate, subsumption, drop-column, direction ×3, channels) | `scripts/export_numbers.py` | `paper/tables/*.tex` |
| Shadow time-series figure | `scripts/fig_shadow_ts.py` | `paper/figures/fig-shadow-ts.pdf` |
| Calibration, fixed-point convergence, shadow-density figures | `scripts/fig_appendix.py` | `paper/figures/fig-{sl-calibration,fp-convergence,shadow-kde}.pdf` |
| Game-tree figure | TikZ source, no data | `paper/figures/ntree-src.tex` |

The appendix's hand-authored tables are verified rather than generated:
`scripts/verify_post1999_table.py` reconciles `tab:post1999` against the raw
CSV (row-by-row, run as part of the replication runner); the coding
corrections in `tab:regan-corrections` are implemented verbatim in
`src/shadow/data/interventions.py` with the original R line citations.

## 4. Data shipped with the package (~700 MB)

**Pipeline outputs** (`data/interim/`) — the analysis-stage inputs the
exhibit scripts read:

| Files | Content |
| --- | --- |
| `cy_shadow_*.parquet` (25) | country-year shadow measures, one per imputation draw |
| `sl_oof_*.parquet` (25) | Stage-1 ensemble out-of-fold predictions |
| `sl_oofpm_*.parquet` (25) + `sl_model_meta.parquet` | per-candidate out-of-fold predictions, ensemble weights, PCA/feature counts — extracted from the fitted-model files by `scripts/distill_sl_models.py` (the fitted estimators themselves are not needed by any exhibit) |
| `sl_spat_conv_*.parquet` (25), `sl_fp_diag.parquet` | fixed-point / burnout convergence diagnostics |
| `dd_int_1_1.parquet` | directed-dyad panel with intervention coding (draw 1_1) |
| `country_year.parquet` | country-year panel (pre-imputation) |

**Stage-2 outputs** (`results/spike/*.parquet`) — leave-one-country-out
predictions and metrics, direction/channel estimates, drop-column and
predictive-significance results, produced by the `scripts/` suite.

**Third-party data on the exhibit path** (`data/raw/`) — all replication
datasets, redistributed with citation; see `data/README.md` for provenance:

- `regan/replication.10.26.01.dta` — Regan (2000) intervention data.
- `regan/post1999_interventions.csv` — this paper's hand-coded post-1999
  extension (original to this package).
- `cunningham/cunningham.dta` — Cunningham (2016) replication data
  (Lake security-hierarchy comparison).
- `gm/conditionalInterventionProbs_replication.csv` — single file from the
  Gibilisco & Montero (2022) Dataverse archive, doi:10.7910/DVN/DKXD2X,
  CC0 1.0 (structural-P5 comparison).

The G&M file is public domain (CC0). The Regan and Cunningham files are
author-distributed journal replication data with no formal license,
redistributed with citation for replication only; they retain their
original terms.

## 5. Full rebuild from raw (documentation; not required for verification)

The complete pipeline reconstructs everything in Section 4 from raw data.
Raw third-party sources not redistributable here are documented, with
versions and download locations, in `data/README.md`. Run notebooks in
order (`scripts/run_notebook.sh` executes one headlessly), then the
distillation and analysis scripts. Approximate runtimes (Apple-silicon
laptop):

| Step | Stage | Time |
| --- | --- | --- |
| `01-country-year` | country-year panel + multiple imputation (5 draws) | ~5 min |
| `02-dyad-data` | directed-dyad expansion (5 × 5 = 25 datasets) | ~45 min |
| `03-interventions` | intervention coding (Regan + post-1999) | ~1 min |
| `04-spatial-weights` | spatial weight matrices | ~20 min |
| `05-stage1-training` | super-learner training, 27 candidates × 25 draws | ~35 h |
| `06-stage1-predictions` | universal predictions, Nash fixed point, aggregation | ~30 min |
| `07-stage2-onset` | onset models, T×P bootstrap | ~4 h |
| `scripts/distill_sl_models.py` | extract exhibit inputs from fitted models | ~5 min |
| `scripts/run_*.sh` suite | OOS gates, direction, drop-column, significance tests | hours, per script |

Stochastic steps (imputation, Stage-1 training, bootstraps) are seeded per
draw in the code. Note that exact bit-reproduction of Stage-1 training
across different BLAS/threading environments is not guaranteed; PSRM's 5%
tolerance applies to the full rebuild. **The verification run in Section 1
is exact** — it involves no retraining.

## 6. Verifying a single number

Every in-prose statistic is a macro in `paper/generated/numbers.tex`,
written only by `scripts/export_numbers.py` from the files in Section 4.
To check one number: find its macro in the manuscript source (e.g.,
`\SLprl` in `paper/sections/constructing.tex`), re-run
`python scripts/export_numbers.py`, and read the regenerated value. No
statistic in the manuscript is hand-typed.

## 7. Directory structure

```text
scripts/            run_replication.sh (entry point), exhibit generators,
                      analysis suite, verification and extraction scripts
src/shadow/         installable package: data construction, models
notebooks/          full pipeline stages 01–07 (09: exploratory figures)
data/raw/           shipped third-party + hand-coded data (Section 4);
                      obtain-separately sources documented in data/README.md
data/interim/       shipped pipeline outputs (Section 4)
results/spike/      shipped Stage-2 outputs
paper/              LaTeX source; generated/, tables/, figures/ are the
                      shipped exhibits the runner compares against
tests/              pytest suite (pytest tests/)
requirements-freeze.txt   exact package versions used for the results
```
