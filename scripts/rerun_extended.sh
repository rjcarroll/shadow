#!/usr/bin/env bash
# Re-run pipeline with extended (post-1999) intervention coding.
# Notebooks 01-02 are unchanged; start from nb03.
#
# Usage: caffeinate -i bash scripts/rerun_extended.sh 2>&1 | tee rerun.log

set -euo pipefail
cd "$(dirname "$0")/.."

VENV=".venv/bin"
NB="$VENV/jupyter-nbconvert --to notebook --execute --inplace \
    --ExecutePreprocessor.timeout=7200 \
    --ExecutePreprocessor.kernel_name=python3"

echo "=== Step 0: Clear cached nb05/nb06 outputs ==="
rm -fv data/interim/sl_model_*.pkl
rm -fv data/interim/sl_oof_*.parquet
rm -fv data/interim/sl_weights_*.parquet
rm -fv data/interim/sl_spat_conv_*.parquet
rm -fv data/interim/sl_cv_metrics.parquet
rm -fv data/interim/sl_preds_*.parquet
rm -fv data/interim/cy_shadow_*.parquet
rm -fv data/interim/sl_fp_diag.parquet
echo

echo "=== Step 1: nb03 — Intervention coding (Regan + post-1999) ==="
$NB notebooks/03-interventions.ipynb
echo

echo "=== Step 2: nb04 — Spatial weights ==="
$NB notebooks/04-spatial-weights.ipynb
echo

echo "=== Step 3: nb05 — Stage 1 training (expanded sample) ==="
$NB notebooks/05-stage1-training.ipynb
echo

echo "=== Step 4: nb06 — Stage 1 predictions ==="
$NB notebooks/06-stage1-predictions.ipynb
echo

echo "=== Step 5: nb07 — Stage 2 onset analysis ==="
$NB notebooks/07-stage2-onset.ipynb
echo

echo "=== Done ==="
date
