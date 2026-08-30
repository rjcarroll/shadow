#!/usr/bin/env bash
# W-ablation: 7 specs x 2 draws (4_2, 1_1) -> Stage-1 retrain + shadow each,
# then the summary table. Idempotent (skips completed combos). spec=full 1_1
# Stage-1 is reused from the validation run; its shadow is still built here.
#
# Run:  nohup bash scripts/run_ablation.sh >/dev/null 2>&1 &
set -uo pipefail
cd "$(dirname "$0")/.."
export OMP_NUM_THREADS=3 MKL_NUM_THREADS=3 OPENBLAS_NUM_THREADS=3 \
       VECLIB_MAXIMUM_THREADS=3 NUMEXPR_NUM_THREADS=3 RF_JOBS=3
LOG="logs/ablation_$(date +%Y%m%d_%H%M%S).log"
echo "ablation start $(date)  (P=4, OMP=3, RF=3; 7 specs x draws 1_1,4_2)" | tee "$LOG"

caffeinate -i bash -c '
  for spec in baseline iso_region iso_igo iso_chain iso_ideal iso_P5 full; do
    for d in "1 1" "4 2"; do echo "$d $spec"; done
  done | xargs -P 4 -I LINE bash scripts/ablate_one.sh LINE
' >>"$LOG" 2>&1

echo "runs done $(date) -- building summary" | tee -a "$LOG"
.venv/bin/python scripts/ablate_metrics.py >>"$LOG" 2>&1
echo "ablation COMPLETE $(date)" | tee -a "$LOG"
tail -20 "$LOG" | grep -A30 "spec" || true
