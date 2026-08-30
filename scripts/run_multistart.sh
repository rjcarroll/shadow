#!/usr/bin/env bash
# Multi-start uniqueness test on one draw (default 1_1), several inits in parallel.
set -uo pipefail
cd "$(dirname "$0")/.."
CY="${1:-1}"; UD="${2:-1}"
LOG="logs/fpms_$(date +%Y%m%d_%H%M%S).log"
echo "fp-multistart start $(date)  draw ${CY}_${UD}  (N_ITER=${FP_N_ITER:-60})" | tee "$LOG"

caffeinate -i bash -c "
  .venv/bin/python scripts/fp_multistart.py $CY $UD standard &
  .venv/bin/python scripts/fp_multistart.py $CY $UD centroid &
  .venv/bin/python scripts/fp_multistart.py $CY $UD zero &
  .venv/bin/python scripts/fp_multistart.py $CY $UD high &
  .venv/bin/python scripts/fp_multistart.py $CY $UD rand1 &
  .venv/bin/python scripts/fp_multistart.py $CY $UD rand2 &
  wait
" >>"$LOG" 2>&1

echo "fp-multistart done $(date)" | tee -a "$LOG"
