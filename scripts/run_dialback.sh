#!/usr/bin/env bash
# Dial-back full re-run: retrain Stage-1 + rebuild the shadow for ALL 25 draws under a
# chosen channel spec, writing CANONICAL outputs (overwriting the current full-channel
# pipeline -- archived first), then rebuild sl_fp_diag. Stage-2 (nb07), export_numbers,
# and the figures are the quicker follow-up, run afterward.
#
# Run:  nohup bash scripts/run_dialback.sh [spec] >/dev/null 2>&1 &
#   spec defaults to iso_chain (= baseline + chain). Use 'baseline' for original-10-only.
# Safe to re-run after a crash: the archive is taken once; FORCE re-overwrites all draws.
set -uo pipefail
cd "$(dirname "$0")/.."
SPEC="${1:-iso_chain}"
export FORCE=1
export OMP_NUM_THREADS=3 MKL_NUM_THREADS=3 OPENBLAS_NUM_THREADS=3 \
       VECLIB_MAXIMUM_THREADS=3 NUMEXPR_NUM_THREADS=3 RF_JOBS=3
LOG="logs/dialback_$(date +%Y%m%d_%H%M%S).log"
ARCH="data/archive/predialback_$(date +%Y%m%d)"
echo "dial-back re-run start $(date)  spec=$SPEC  (P=5, OMP=3, RF=3)" | tee "$LOG"

# 1. archive the current (full-channel) canonical outputs ONCE, before overwriting
if [ ! -e "$ARCH/sl_model_1_1.pkl" ]; then
  mkdir -p "$ARCH"
  cp data/interim/sl_model_*.pkl data/interim/sl_oof_*.parquet \
     data/interim/sl_weights_*.parquet data/interim/cy_shadow_*.parquet \
     data/interim/sl_preds_*.parquet data/interim/sl_fp_diag.parquet \
     data/interim/sl_cv_metrics.parquet "$ARCH"/ 2>/dev/null || true
  n=$(ls "$ARCH"/sl_model_*.pkl 2>/dev/null | wc -l | tr -d ' ')
  echo "archived $n current models -> $ARCH" | tee -a "$LOG"
  [ "$n" -ge 25 ] || { echo "ABORT: archive incomplete ($n/25); not overwriting" | tee -a "$LOG"; exit 1; }
else
  echo "archive $ARCH already present -- skipping re-archive" | tee -a "$LOG"
fi

# 2. retrain Stage-1 (canonical) + rebuild shadow, all 25 draws, 5-way parallel
caffeinate -i bash -c '
  for cy in 1 2 3 4 5; do for ud in 1 2 3 4 5; do echo "$cy $ud '"$SPEC"'"; done; done |
    xargs -P 5 -I LINE bash scripts/dialback_one.sh LINE
' >>"$LOG" 2>&1

# 3. rebuild sl_fp_diag from the per-draw convergence diagnostics
.venv/bin/python - >>"$LOG" 2>&1 <<'PY'
import pandas as pd, glob
fd = sorted(glob.glob('data/interim/fpr_diag_*.parquet'))
pd.concat([pd.read_parquet(f) for f in fd], ignore_index=True).to_parquet(
    'data/interim/sl_fp_diag.parquet', index=False)
print('sl_fp_diag rebuilt from', len(fd), 'draws')
PY

echo "dial-back re-run COMPLETE $(date)" | tee -a "$LOG"
echo "models $(ls data/interim/sl_model_*.pkl|wc -l|tr -d ' ')/25  shadows $(ls data/interim/cy_shadow_*.parquet|wc -l|tr -d ' ')/25" | tee -a "$LOG"
echo "NEXT (quicker, run after): nb07 Stage-2; scripts/export_numbers.py; regen figures" | tee -a "$LOG"
