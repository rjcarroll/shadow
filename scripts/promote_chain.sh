#!/usr/bin/env bash
# Promote the already-computed baseline+chain (iso_chain) outputs from the ablation
# tag to CANONICAL, making baseline+chain the pipeline. Pure copy -- no re-training.
# Archives the current full-channel canonical first (reversible), then rebuilds
# sl_fp_diag + sl_cv_metrics. Self-aborts rather than overwrite if anything is missing.
set -uo pipefail
cd "$(dirname "$0")/.."
SPEC=iso_chain
ABL=data/interim/ablation
ARCH="data/archive/fullmodel_$(date +%Y%m%d)"
LOG="logs/promote_$(date +%Y%m%d_%H%M%S).log"
echo "promote $SPEC -> canonical, start $(date)" | tee "$LOG"

# 0. sanity: all 25 iso_chain models present
nsrc=$(ls "$ABL"/sl_model_${SPEC}_*.pkl 2>/dev/null | wc -l | tr -d ' ')
[ "$nsrc" -ge 25 ] || { echo "ABORT: only $nsrc/25 $SPEC models in $ABL" | tee -a "$LOG"; exit 1; }

# 1. archive current full-channel canonical (once)
if [ ! -e "$ARCH/sl_model_1_1.pkl" ]; then
  mkdir -p "$ARCH"
  cp data/interim/sl_model_*.pkl data/interim/sl_oof_*.parquet data/interim/sl_weights_*.parquet \
     data/interim/cy_shadow_*.parquet data/interim/sl_preds_*.parquet \
     data/interim/sl_fp_diag.parquet data/interim/sl_cv_metrics.parquet "$ARCH"/ 2>/dev/null || true
  narch=$(ls "$ARCH"/sl_model_*.pkl 2>/dev/null | wc -l | tr -d ' ')
  echo "archived $narch full-model draws -> $ARCH" | tee -a "$LOG"
  [ "$narch" -ge 25 ] || { echo "ABORT: archive incomplete ($narch/25); canonical untouched" | tee -a "$LOG"; exit 1; }
else
  echo "archive $ARCH already present -- skipping re-archive" | tee -a "$LOG"
fi

# 2. copy iso_chain outputs -> canonical (strip the spec tag)
for cy in 1 2 3 4 5; do for ud in 1 2 3 4 5; do
  for kind in sl_model.pkl sl_oof.parquet sl_weights.parquet cy_shadow.parquet sl_preds.parquet fpr_diag.parquet; do
    base="${kind%%.*}"; ext="${kind#*.}"
    src="$ABL/${base}_${SPEC}_${cy}_${ud}.${ext}"
    if [ -e "$src" ]; then cp "$src" "data/interim/${base}_${cy}_${ud}.${ext}"
    else echo "MISSING $src" | tee -a "$LOG"; fi
  done
done; done
echo "copied 25 draws -> canonical" | tee -a "$LOG"

# 3. rebuild aggregates from the promoted per-draw files
.venv/bin/python - >>"$LOG" 2>&1 <<'PY'
import pandas as pd, glob, joblib, re
fd = sorted(glob.glob('data/interim/fpr_diag_*.parquet'))
pd.concat([pd.read_parquet(f) for f in fd], ignore_index=True).to_parquet('data/interim/sl_fp_diag.parquet', index=False)
rows = []
for f in sorted(glob.glob('data/interim/sl_model_*.pkl')):
    cy, ud = re.search(r'sl_model_(\d)_(\d)\.pkl', f).groups()
    m = joblib.load(f)['component_metrics'].copy(); m['cy'], m['ud'] = int(cy), int(ud); rows.append(m)
pd.concat(rows, ignore_index=True).to_parquet('data/interim/sl_cv_metrics.parquet', index=False)
print(f'rebuilt sl_fp_diag ({len(fd)} draws) + sl_cv_metrics')
PY
echo "promote COMPLETE $(date)" | tee -a "$LOG"
