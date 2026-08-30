#!/usr/bin/env bash
# Tier-2 data rebuild: nb01 (adds other_ongoing_wars; canonical outputs) with
# the C1 imputation-equality gate, then nb02→nb03→nb04 into data/interim/lag1/
# (t-1 information-set dd chain).  ~30 min total.  Requires the prelag archive.
# Plan: i-went-home-so-wiggly-quill.md.  Next step: scripts/run_lag1.sh pilot
set -euo pipefail
cd "$(dirname "$0")/.."
ARCH=$(ls -d data/archive/prelag_* 2>/dev/null | tail -1)
[ -n "$ARCH" ] && [ -e "$ARCH/cy_imputed_1.parquet" ] \
  || { echo "ABORT: prelag archive with cy_imputed files required"; exit 1; }

echo "=== nb01 (canonical: country_year + cy_imputed regen) $(date) ==="
./scripts/run_notebook.sh notebooks/01-country-year.ipynb

echo "=== C1: cy_imputed equality vs archive ==="
.venv/bin/python - "$ARCH" <<'PY'
import sys
import pandas as pd
arch = sys.argv[1]
for i in range(1, 6):
    new = pd.read_parquet(f"data/interim/cy_imputed_{i}.parquet")
    old = pd.read_parquet(f"{arch}/cy_imputed_{i}.parquet")
    try:
        pd.testing.assert_frame_equal(new, old)
        print(f"cy_imputed_{i}: identical")
    except AssertionError as e:
        print(f"cy_imputed_{i}: DRIFT -- restoring archived copy")
        old.to_parquet(f"data/interim/cy_imputed_{i}.parquet", index=False)
# country_year gains the new column; onset/ongoing invariants
cy = pd.read_parquet("data/interim/country_year.parquet")
assert "other_ongoing_wars" in cy.columns
assert (cy.other_ongoing_wars <= cy.ongoing_wars).all()
incy = cy[cy.inCY]
print(f"onsets 1946-2014: {int(incy.onset.sum())} (expect 191)")
print(f"corr(onset, ongoing_wars)       = {incy[['onset','ongoing_wars']].corr().iloc[0,1]:+.3f}")
print(f"corr(onset, other_ongoing_wars) = {incy[['onset','other_ongoing_wars']].corr().iloc[0,1]:+.3f}")
PY

export SHADOW_DD_DIR="../data/interim/lag1"
mkdir -p data/interim/lag1
echo "=== nb02 (t-1 dd chain -> lag1) $(date) ==="
./scripts/run_notebook.sh notebooks/02-dyad-data.ipynb
echo "=== nb03 (interventions) $(date) ==="
./scripts/run_notebook.sh notebooks/03-interventions.ipynb
echo "=== nb04 (spatial lags) $(date) ==="
./scripts/run_notebook.sh notebooks/04-spatial-weights.ipynb

echo "=== C3 cross-check: lag1 label counts must match canonical exactly ==="
.venv/bin/python - <<'PY'
import pandas as pd
new = pd.read_parquet("data/interim/lag1/dd_int_1_1.parquet", columns=["onset_A", "intervention"])
old = pd.read_parquet("data/interim/dd_int_1_1.parquet", columns=["onset_A", "intervention"])
for name, df in [("lag1", new), ("canonical", old)]:
    on = df[df.onset_A == 1]
    print(f"{name}: onset rows {len(on):,}, gov {(on.intervention==1).sum()}, opp {(on.intervention==2).sum()}")
assert (new[new.onset_A == 1].intervention.value_counts().sort_index().values
        == old[old.onset_A == 1].intervention.value_counts().sort_index().values).all(), \
    "label counts differ -- intervention coding must be covariate-independent"
print("C3 PASSED")
PY
echo "=== data rebuild COMPLETE $(date); next: ./scripts/run_lag1.sh pilot ==="
