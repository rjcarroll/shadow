#!/bin/zsh
# Tier-2 lag1 rebuild driver: Stage-1 retrain (iso_chain spec, t-1 dd chain) +
# universal FP for 25 draws, then aggregate rebuild -- all inside
# data/interim/lag1/ (canonical names; nothing canonical is touched until
# scripts/promote_lag1.sh).  Plan: i-went-home-so-wiggly-quill.md.
#
# Prereqs: nb01+nb02/03/04 re-run with SHADOW_DD_DIR=data/interim/lag1 so
# lag1/ holds 25 dd_spat files.  Proven parallelism: P=5 x 3 threads
# (run_rerun.sh header: 8-way was slower, memory-bandwidth-bound).
#
# Usage:  ./scripts/run_lag1.sh            # full 25-draw fan-out
#         ./scripts/run_lag1.sh pilot      # draw (1,1) only (checkpoint C5)
set -uo pipefail
cd "$(dirname "$0")/.."
mkdir -p logs
export SHADOW_INTERIM="$PWD/data/interim/lag1"
export OMP_NUM_THREADS=3 MKL_NUM_THREADS=3 OPENBLAS_NUM_THREADS=3
export VECLIB_MAXIMUM_THREADS=3 NUMEXPR_NUM_THREADS=3
export RF_JOBS=3
export FP_MAX_ITER=60 FP_TOL=1e-4

ndd=$(ls "$SHADOW_INTERIM"/dd_spat_*_*.parquet 2>/dev/null | wc -l | tr -d ' ')
[ "$ndd" -ge 25 ] || { echo "ABORT: only $ndd/25 dd_spat files in lag1 -- run nb02-04 first"; exit 1; }

one_draw() {
  .venv/bin/python scripts/ablate_stage1.py "$1" "$2" iso_chain canonical \
    && .venv/bin/python scripts/fp_rerun.py "$1" "$2"
}

if [ "${1:-}" = "pilot" ]; then
  echo "=== lag1 pilot (1,1) start $(date) ==="
  one_draw 1 1
  echo "=== lag1 pilot done $(date) ==="
  exit 0
fi

echo "=== lag1 fan-out start $(date) ==="
for cy in 1 2 3 4 5; do for ud in 1 2 3 4 5; do echo "$cy $ud"; done; done \
  | SHADOW_INTERIM="$SHADOW_INTERIM" xargs -P 5 -L 1 sh -c \
      'if [ -e "$SHADOW_INTERIM/cy_shadow_$0_$1.parquet" ]; then echo "skip draw $0_$1 (complete)"; \
       else .venv/bin/python scripts/ablate_stage1.py $0 $1 iso_chain canonical \
         && .venv/bin/python scripts/fp_rerun.py $0 $1; fi'
echo "=== lag1 draws done $(date); rebuilding aggregates ==="

.venv/bin/python - <<'PY'
import pandas as pd, glob, joblib, re, os
L = os.environ["SHADOW_INTERIM"]
fd = sorted(glob.glob(f"{L}/fpr_diag_[1-5]_[1-5].parquet"))
pd.concat([pd.read_parquet(f) for f in fd], ignore_index=True).to_parquet(
    f"{L}/sl_fp_diag.parquet", index=False)
rows = []
for f in sorted(glob.glob(f"{L}/sl_model_[1-5]_[1-5].pkl")):
    cy, ud = re.search(r"sl_model_(\d)_(\d)\.pkl", f).groups()
    m = joblib.load(f)["component_metrics"].copy()
    m["cy"], m["ud"] = int(cy), int(ud)
    rows.append(m)
pd.concat(rows, ignore_index=True).to_parquet(f"{L}/sl_cv_metrics.parquet", index=False)
print(f"rebuilt sl_fp_diag ({len(fd)} draws) + sl_cv_metrics ({len(rows)} models)")
PY
echo "=== lag1 COMPLETE $(date) ==="
date > "$SHADOW_INTERIM/FANOUT_COMPLETE"
