# Next Session TODO

## Immediate: finish notebook 02 execution

Notebook 02 was edited and partially run but not fully completed.
Run the following from the repo root (takes ~40–50 min; requires miceforest on ~600k rows):

```bash
python3 -m jupyter nbconvert --to notebook --execute --inplace \
  --ExecutePreprocessor.timeout=7200 \
  notebooks/02-dyad-data.ipynb
```

**What it will produce**: `data/interim/dd_{cy}_{ud}.parquet` for cy ∈ 1–5, ud ∈ 1–5 (25 files).
The first 5 files (`dd_1_1` through `dd_1_5`) may already exist from a prior partial run; the
notebook will overwrite them cleanly (the main loop deletes stale `dd_{i}.parquet` files first).

**Expected output** (truncated):
```
── CY imputation 1/5 ──
  Building undirected dyads ... 599,365 UD rows
  Merging auxiliary variables ... done
  Imputing UD variables (5 datasets) ... done
  → dd_1_1.parquet  (1,198,730 rows, 107 cols)
  ...
All 25 DD files written.

DD files produced: 25  (expected 25)
Trade coverage (ud_A_biImports): 100%  (should be 100% after imputation)
Peace score coverage: 100%  (should be 100% after imputation)
Rows with imputation variance across UD datasets: [>0]
✓ Validation complete.
```

---

## Up next: notebook 03 — Intervention Coding

**File**: `notebooks/03-interventions.ipynb`
**Module**: `src/shadow/data/interventions.py`
**Reference R script**: `zzz-old_version/Paper-Shadow/R/08-addInterventions.R`

Port the Regan intervention coding onto each of the 25 DD files.

**Data source**: `data/raw/regan/replication.10.26.01.dta` (Regan 2002 replication data)

**Key steps**:
1. Read `replication.10.26.01.dta` via pyreadstat
2. Apply all 89 per-conflict coding corrections from the R script (verbatim, with comments
   citing original line numbers)
3. Match each intervention to a civil war onset dyad-year via a ±2 year window
4. Code outcome: 0 = no intervention, 1 = government-biased, 2 = opposition-biased
5. Merge onto all 25 DD files → write `data/interim/dd_int_{cy}_{ud}.parquet`

**Validation targets** (from original paper):
- Total military interventions: **118**
- Government-biased: **66**, opposition-biased: **52**
- Unique intervening states: **53**
- Wars with no intervention: **59 / 111**

---

## After that: notebook 04 — Spatial Weights

**File**: `notebooks/04-spatial-weights.ipynb`
**Module**: `src/shadow/data/spatial.py`
**Reference R scripts**: `10-makeWpol.R`, `11-addSpatial.R`

Build polity-based spatial weight matrices and add spatial lag variables.

**Key steps**:
1. For each year 1946–2014: W[i,j] = 1 / |polity2_i − polity2_j| (0 on diagonal; 0 if equal)
2. Row-normalise; store as `scipy.sparse.csr_matrix`
3. Filter 1990 Soviet anomaly (ccode 678)
4. Compute 10 spatial lag variables per DD file:
   spat_gov, spat_opp, spat_US_G, spat_USSR_G, spat_US_O, spat_USSR_O,
   spat_US_USRG, spat_US_USRO, spat_USR_USG, spat_USR_USO
5. Append spatial lags to each of the 25 intervention-coded DD files

**Validation**: W matrix rows should sum to 1.0 (within floating point tolerance).

---

## Project context

- Repo: https://github.com/rjcarroll/shadow (private)
- Working dir: `/Users/rjc/portfolio/shadow`
- Install: `pip install -e ".[dev]"` (already done, just activate the environment)
- Data: `data/` is gitignored; raw files must already be present on the machine
- Memory file: `.claude/projects/…/memory/MEMORY.md` (auto-loaded by Claude Code)
