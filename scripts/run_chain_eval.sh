#!/usr/bin/env bash
# Evaluate baseline+chain (iso_chain): Stage-1 retrain + shadow for ALL 25 draws,
# written to the ablation tag (data/interim/ablation/) -- NON-canonical. The current
# full-channel pipeline (data/interim/sl_model_*, cy_shadow_*, ...) is left untouched,
# so reverting to it (or to baseline) costs nothing. 1_1 and 4_2 are reused from the
# ablation (skipped). Promotion to canonical, Stage-2, etc. are deliberately NOT done.
set -uo pipefail
cd "$(dirname "$0")/.."
export OMP_NUM_THREADS=3 MKL_NUM_THREADS=3 OPENBLAS_NUM_THREADS=3 \
       VECLIB_MAXIMUM_THREADS=3 NUMEXPR_NUM_THREADS=3 RF_JOBS=3
LOG="logs/chaineval_$(date +%Y%m%d_%H%M%S).log"
echo "iso_chain (baseline+chain) Stage-1 + shadow, all 25 draws, NON-canonical, start $(date)" | tee "$LOG"
caffeinate -i bash -c '
  for cy in 1 2 3 4 5; do for ud in 1 2 3 4 5; do echo "$cy $ud iso_chain"; done; done |
    xargs -P 5 -I LINE bash scripts/ablate_one.sh LINE
' >>"$LOG" 2>&1
echo "COMPLETE $(date)" | tee -a "$LOG"
echo "models $(ls data/interim/ablation/sl_model_iso_chain_*.pkl 2>/dev/null | wc -l | tr -d ' ')/25  shadows $(ls data/interim/ablation/cy_shadow_iso_chain_*.parquet 2>/dev/null | wc -l | tr -d ' ')/25" | tee -a "$LOG"
