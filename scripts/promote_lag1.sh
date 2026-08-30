#!/usr/bin/env bash
# Promote the lag1 rebuild (t-1 information-set dd chain + iso_chain Stage-1 +
# universal FP shadows) from data/interim/lag1/ to CANONICAL.  Modeled on
# promote_chain.sh.  Pure copy -- no re-training.  Requires the prelag archive
# (made 2026-07-10) so the promotion is reversible.  Run AFTER checkpoint C7
# (plan: i-went-home-so-wiggly-quill.md).
set -uo pipefail
cd "$(dirname "$0")/.."
LAG=data/interim/lag1
ARCH=$(ls -d data/archive/prelag_* 2>/dev/null | tail -1)
LOG="logs/promote_lag1_$(date +%Y%m%d_%H%M%S).log"
echo "promote lag1 -> canonical, start $(date)" | tee "$LOG"

# 0. sanity: complete lag1 build
for pat in "dd_spat_*_*.parquet" "sl_model_*_*.pkl" "cy_shadow_*_*.parquet" "sl_preds_*_*.parquet"; do
  n=$(ls $LAG/${pat} 2>/dev/null | wc -l | tr -d ' ')
  [ "$n" -ge 25 ] || { echo "ABORT: only $n/25 $pat in $LAG" | tee -a "$LOG"; exit 1; }
done
[ -e "$LAG/sl_fp_diag.parquet" ] || { echo "ABORT: lag1 aggregates missing (run_lag1.sh tail)" | tee -a "$LOG"; exit 1; }

# 1. archive must exist (made before the rebuild started)
[ -n "$ARCH" ] && [ -e "$ARCH/sl_model_1_1.pkl" ] && [ -e "$ARCH/dd_spat_1_1.parquet" ] \
  || { echo "ABORT: prelag archive missing/incomplete ($ARCH)" | tee -a "$LOG"; exit 1; }
echo "archive verified: $ARCH" | tee -a "$LOG"

# 2. copy lag1 -> canonical (dd chain INCLUDED: export_numbers/tab:interveners
#    and the host-block probes read dd files from canonical paths)
cp $LAG/dd_[1-5]_[1-5].parquet $LAG/dd_int_[1-5]_[1-5].parquet \
   $LAG/dd_spat_[1-5]_[1-5].parquet \
   $LAG/sl_model_[1-5]_[1-5].pkl $LAG/sl_oof_[1-5]_[1-5].parquet \
   $LAG/sl_weights_[1-5]_[1-5].parquet $LAG/sl_preds_[1-5]_[1-5].parquet \
   $LAG/cy_shadow_[1-5]_[1-5].parquet $LAG/fpr_diag_[1-5]_[1-5].parquet \
   $LAG/sl_fp_diag.parquet $LAG/sl_cv_metrics.parquet \
   data/interim/ | tee -a "$LOG"
echo "copied lag1 -> canonical" | tee -a "$LOG"
echo "promote COMPLETE $(date)" | tee -a "$LOG"
