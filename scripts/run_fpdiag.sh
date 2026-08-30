#!/usr/bin/env bash
# Launch the fixed-point diagnostic on the selected capped/contrast draws, in
# parallel under caffeinate. Detach via: python -c "subprocess.Popen(..., start_new_session=True)".
set -uo pipefail
cd "$(dirname "$0")/.."
LOG="logs/fpdiag_$(date +%Y%m%d_%H%M%S).log"
echo "fp-diagnostic start $(date)  (N_ITER=${FP_N_ITER:-40})" | tee "$LOG"

caffeinate -i bash -c '
  .venv/bin/python scripts/fp_diagnostic.py 1 1 1.0 &   # worst capped
  .venv/bin/python scripts/fp_diagnostic.py 4 3 1.0 &   # 2nd worst
  .venv/bin/python scripts/fp_diagnostic.py 3 4 1.0 &   # drifted up after touching tol
  .venv/bin/python scripts/fp_diagnostic.py 5 2 1.0 &   # slow converger (contrast)
  .venv/bin/python scripts/fp_diagnostic.py 1 1 0.5 &   # DAMPED arm on the worst
  wait
' >>"$LOG" 2>&1

echo "fp-diagnostic done $(date)" | tee -a "$LOG"
