#!/usr/bin/env bash
# Overnight re-run of nb06's universal fixed point for all 25 draws, standard init
# (the realized low-coordination basin), raised ceiling + tighter tolerance.
#
# Tuned to 5 workers x 3 threads (= 15 of 18 cores): the predict step is
# memory-bandwidth-bound, so 8-way parallel actually ran SLOWER per draw. Fewer,
# fatter workers dodge the contention. Redoes all 25 cleanly at FP_TOL=1e-4
# (overwriting any partials), then rebuilds sl_fp_diag from the per-draw diags.
#
# Run:  nohup bash scripts/run_rerun.sh >/dev/null 2>&1 &   (detached; logs to logs/)
#  or:  bash scripts/run_rerun.sh                            (in a terminal you leave open)
set -uo pipefail
cd "$(dirname "$0")/.."
export OMP_NUM_THREADS=3 MKL_NUM_THREADS=3 OPENBLAS_NUM_THREADS=3 \
       VECLIB_MAXIMUM_THREADS=3 NUMEXPR_NUM_THREADS=3 FP_MAX_ITER=60 FP_TOL=1e-4
LOG="logs/fprerun_$(date +%Y%m%d_%H%M%S).log"
echo "fp_rerun start $(date)  (P=5, 3 threads, cap 60, tol 1e-4, all 25 draws)" | tee "$LOG"

caffeinate -i bash -c '
  for cy in 1 2 3 4 5; do for ud in 1 2 3 4 5; do echo "$cy $ud"; done; done |
    xargs -P 5 -I PAIR sh -c ".venv/bin/python scripts/fp_rerun.py PAIR"
' >>"$LOG" 2>&1

echo "draws done $(date) -- rebuilding sl_fp_diag" | tee -a "$LOG"
.venv/bin/python -c "
import pandas as pd, glob
fd = sorted(glob.glob('data/interim/fpr_diag_*.parquet'))
pd.concat([pd.read_parquet(f) for f in fd], ignore_index=True).to_parquet('data/interim/sl_fp_diag.parquet', index=False)
print('sl_fp_diag rebuilt from', len(fd), 'draws')
" >>"$LOG" 2>&1

echo "fp_rerun COMPLETE $(date)" | tee -a "$LOG"
n=$(ls data/interim/cy_shadow_*.parquet 2>/dev/null | wc -l | tr -d ' ')
echo "cy_shadow files: $n/25" | tee -a "$LOG"
