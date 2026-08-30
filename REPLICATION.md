# Replication Package — *Fighting in the Shadow of Intervention* (PSRM-2026-0115)

> **Status: under construction (2026-06-17).** Built incrementally alongside the verification audit (`notes/verification-ledger.md`). Sections marked **[TODO]** are not yet verified — do not rely on them yet. Verified content is marked ✓ with a pointer to the ledger.

## 1. Overview

A two-stage learned-proxy design. **Stage 1:** a machine-learning ensemble predicts the probability and direction (government- vs. opposition-biased) of intervention for each directed dyad-year. Predictions are aggregated to country-year "shadow" measures `E^G` / `E^O` under a Nash fixed-point condition. **Stage 2:** a logit relates the shadow to civil-war onset. This package reproduces every table, figure, and reported statistic from source.

## 2. Reproducibility levels

- **Level 1 — exhibits from committed outputs.** Re-derive every reported number/table/figure from the committed intermediate outputs (`data/interim/`, `results/`). Cheap (minutes). The generated macros (`paper/generated/numbers.tex`) and tables (`paper/tables/`) are built from these, so the page equals the output *by construction*.
- **Level 2 — full re-run from raw.** Rebuild the intermediate outputs from `data/raw/` via nb01–09. Heavy; the Stage-1 ensemble (nb05) is the bottleneck (**[TODO: confirm runtime]** — order of a day+).

## 3. Environment

- Python ≥ 3.11 (developed on 3.14). Dependencies in `pyproject.toml`; recreate `.venv/` with `uv sync` (or `pip install -e .`).
- Key libraries: pandas, numpy, scipy, scikit-learn, statsmodels, miceforest, joblib, pyreadstat, matplotlib/seaborn.
- **[TODO: ship an exact lockfile (`uv.lock` / pinned versions) with the package.]**

## 4. Data

**Ships with the package (ours):** `data/raw/regan/post1999_interventions.csv` (hand-coded post-1999 extension) and the Regan coding corrections (applied in nb03; documented in appendix `tab:regan-corrections`).

**Third-party sources** — obtain separately; **[TODO: confirm exact version + redistribution terms for each]**. Versions below are *as stated in the manuscript appendix* (`tab:cy-vars`, `tab:dyad-vars`) and must be confirmed against the actual raw files.

| `data/raw/` dir | Dataset | Version (per paper) | Redistribution |
|---|---|---|---|
| `cow` | COW Intra-State War (onset universe) | v5.1 | [TODO] |
| `nmc` | COW National Material Capabilities (CINC) | v6 | [TODO] |
| `polity` | Polity | [TODO] | [TODO] |
| `vdem` | V-Dem | v15 | [TODO] |
| `regan` | Regan, *Civil Wars and Foreign Powers* | [TODO] | [TODO] |
| `atop` | ATOP alliances | [TODO] | [TODO] |
| `icow` | ICOW territorial claims | [TODO] | [TODO] |
| `epr` / `ethnic` | EPR / ethnic composition | [TODO] | [TODO] |
| `doe` | directed-dyad capabilities (DOE) | [TODO] | [TODO] |
| `rivalries` | strategic rivalries | [TODO] | [TODO] |
| `un-ideals` | UN voting ideal points | [TODO] | [TODO] |
| `cunningham` | Lake security hierarchy (Cunningham 2016) | [TODO] | [TODO] |
| `fl` | Fearon & Laitin 2003 replication | [TODO] | [TODO] |
| `peacedata` / `sullivan-karreth` | auxiliary | [TODO] | [TODO] |
| `replication/` | Gibilisco & Montero structural P5 (dataverse) | [TODO] | dataverse |

## 5. Pipeline

Run in order. **[TODO: confirm per-step runtimes.]**

| Step | Produces | Output |
|---|---|---|
| nb01 country-year | CY panel + 5 imputations | `data/interim/cy_imputed_{1..5}.parquet` |
| nb02 dyads | directed-dyad files | `data/interim/dd_{cy}_{ud}.parquet` |
| nb03 interventions | intervention coding merged to dyads | `data/interim/dd_int_*.parquet` |
| nb04 spatial | spatial lags | `data/interim/dd_spat_*.parquet` |
| nb05 Stage-1 ensemble | super-learner OOF preds + weights | `data/interim/sl_*.parquet` |
| nb06 shadow | country-year `E^G`/`E^O` (agg + disaggregated) | `data/interim/cy_shadow_*.parquet` |
| nb07 Stage-2 onset | onset logit, T×P bootstrap, FE | `results/stage2_*.parquet` |
| nb09 figures | paper figures | `paper/figures/*.pdf` |
| `scripts/*.py` | OOS LOCO, channels, subsumption, diagnostics, FE bootstrap, G&M correlation | `results/`, `results/spike/*` |

**[TODO: commit the 13 untracked `scripts/*.py` — they produce §3 exhibits and must ship with the package.]**

## 6. Exhibit map

Every paper exhibit → the code/output that produces it → the generated artifact. Grows row-by-row with the audit (authoritative status in `notes/verification-ledger.md`).

| Exhibit | Manuscript loc | Source | Generated artifact | Verified |
|---|---|---|---|---|
| Data/coding counts (455; 254 = 150/104; 51; 58; 110/43%; 53; 153/191) | constructing.tex §2 | nb03 + `dd_int_*` + `src/shadow/data/interventions.py` | [TODO: `numbers.tex` macros] | ✓ (ledger §2) |
| `tab:interveners` | constructing.tex | nb03 intervention table | [TODO: `tables/interveners.tex`] | [TODO] |
| `tab:regan-corrections` | appendix | nb03 / corrections list | [TODO] | [TODO] |
| `tab:post1999` | appendix | `post1999_interventions.csv` | [TODO] | [TODO] |
| `tab:cy-vars`, `tab:dyad-vars` | appendix | nb01 / nb02 schemas | [TODO] | [TODO] |
| `tab:stage1-perf`, `tab:sl-components` | constructing.tex / appendix | nb05 (`sl_cv_metrics`, `sl_weights`) | [TODO] | [TODO] |
| `tab:shadow-dyads`; r=0.29/0.53 | constructing.tex | nb06 + `gm_correlation.py` | [TODO] | [TODO] |
| `tab:coefs`; OOS; T×P; subsumption (`tab:cunningham`, `tab:lr-tests`) | decision.tex / appendix | nb07 + `scripts/*` | [TODO] | [TODO] (held: §3) |
| Figures (shadow ts/kde, topfit, calibration, …) | various | nb09 | `paper/figures/*.pdf` | [TODO] |

## 7. How to verify a single number

**[TODO — once the export script lands:** "run `scripts/export_numbers.py`; the value written to `paper/generated/numbers.tex` is the source value, and the manuscript `\input`s it, so they cannot diverge."]

## 8. Directory structure

```
data/{raw,interim,processed}   raw sources · committed intermediates · (empty)
notebooks/                     nb01–09 (committed pipeline)
scripts/                       add-on analyses (OOS, channels, subsumption, …) [some untracked — TODO commit]
src/shadow/                    package code (data builders, models)
paper/{sections,tables,generated,figures}   manuscript · generated tables · generated macros · figures
results/                       Stage-2 outputs (+ results/spike/)
replication/                   third-party replication inputs (e.g., G&M dataverse)
notes/                         internal (verification ledger, working notes)  [gitignored]
readings/                      cited PDFs (for the citation audit)             [gitignored]
```
